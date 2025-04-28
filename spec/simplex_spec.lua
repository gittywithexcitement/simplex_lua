-- Tolerance for nearly-equal comparisons: 2^-20
local TOLERANCE = 2 ^ -20

--- Asserts that two numbers are nearly equal within an absolute or relative error.
--- Prints an error message if they are not.
--- @param computed number
--- @param expected number
local function assert_nearly_equal(computed, expected)
  local abs_error = math.abs(computed - expected)
  local nan = 0 / 0
  local rel_error = abs_error / (math.abs(expected) > 0 and math.abs(expected) or nan)

  if not (abs_error < TOLERANCE or rel_error < TOLERANCE) then
    error(string.format(
      "Values not nearly equal. Computed: %.10f, Expected: %.10f. Abs error: %.10g, Rel error: %.10g",
      computed, expected, abs_error, rel_error
    ), 2)
  end
end

describe("simplex helpers ", function()
  it("do_pivot correctly normalizes pivot row and eliminates others", function()
    local dbg = require("simplex").debug
    -- Tableau: 2 constraint rows + RHS
    -- [ 2  1 | 4 ]
    -- [ 1 -1 | 1 ]
    -- Pick pivot at (1,1) so that row1 → [1 0.5 | 2]
    -- and row2 → [0 -1.5 | -1]
    local T = {
      { 2, 1,  4 },
      { 1, -1, 1 },
    }
    dbg.do_pivot(T, 1, 1)
    -- After pivot:
    -- T[1] ≈ {1, 0.5, 2}
    assert.are.equal(1, T[1][1])
    assert.are.equal(0.5, T[1][2])
    assert.are.equal(2, T[1][3])
    -- Row 2: original row2 – factor·row1 = {1,-1,1} – 1·{1,0.5,2} = {0,-1.5,-1}
    assert.are.equal(0, T[2][1])
    assert.are.equal(-1.5, T[2][2])
    assert.are.equal(-1, T[2][3])
  end)


  it("choose_entering returns nil when no negative reduced costs", function()
    -- choose_entering returns nil when no reduced cost < –tol (i.e. already optimal)
    local dbg = require("simplex").debug
    local T = {
      { 0.1, 0.0, 1.0 } -- cost row only, all ≥ 0
    }
    assert.is_nil(dbg.choose_entering(T))
  end)

  it("choose_leaving returns nil when no positive pivot entries", function()
    -- choose_leaving returns nil when no positive pivot candidates (i.e. unbounded)
    local dbg = require("simplex").debug
    -- Two constraint rows, but column 1 entries are non-positive
    local T = {
      { 0,  5 },
      { -1, 6 },
      {} -- cost row ignored
    }
    assert.is_nil(dbg.choose_leaving(T, 1))
  end)

  it("build_phase1 reuses slack columns when costs==0 and adds no artificials", function()
    local dbg = require("simplex").debug
    -- A = I₂, b = {3,4}, costs = {0,0} ⇒ slack cols are reused
    local A = { { 1, 0 }, { 0, 1 } }
    local b = { 3, 4 }
    local costs = { 0, 0 }
    local T1, basis, art_cols = dbg.build_phase1(A, b, costs)
    assert.are.same({}, art_cols)    -- no artificials injected
    assert.are.same({ 1, 2 }, basis) -- basic vars are cols 1 & 2
    assert.are.equal(3, T1[1][3])    -- RHS carried through
    assert.are.equal(4, T1[2][3])
  end)

  it("build_phase1 reuses some slacks and adds artificials as needed", function()
    local dbg                 = require("simplex").debug
    -- A has two cols; only col1 is a perfect slack (unit & zero cost)
    local A                   = { { 1, 1 }, { 0, 1 } }
    local b                   = { 3, 4 }
    local costs               = { 0, 0 }
    local T1, basis, art_cols = dbg.build_phase1(A, b, costs)
    -- col1 reused, col2 not a slack ⇒ one artificial at col3
    assert.are.same({ 3 }, art_cols)
    -- basis[1]=1 (slack), basis[2]=3 (artificial)
    assert.are.same({ 1, 3 }, basis)
    -- And RHSs carried through correctly
    assert.are.equal(3, T1[1][#T1[1]])
    assert.are.equal(4, T1[2][#T1[2]])
  end)


  it("build_phase1 flips negative b and adds artificials when needed", function()
    local dbg = require("simplex").debug
    -- Single constraint: -x ≤ -2 ⇒ flip to x ≤ 2, costs non-zero ⇒ needs artificial
    local A = { { -1 } }
    local b = { -2 }
    local costs = { 1 }
    local T1, basis, art_cols = dbg.build_phase1(A, b, costs)
    -- After flip b = 2, A = {{1}}
    -- No slack cols (costs[1]!=0) ⇒ one artificial at col 2
    assert.are.same({ 2 }, art_cols)
    assert.are.same({ 2 }, basis)
    assert.are.equal(1, T1[1][2]) -- art column identity entry
    assert.are.equal(2, T1[1][3]) -- RHS = |b|
  end)

  it("purge_artificial pivots out artificial basis and removes those columns", function()
    -- Pivoting out an artificial that remains in the basis (i.e. when artificial stayed basic
    -- after Phase I).
    --
    -- Rebuilding the tableau without artificial columns.
    local dbg = require("simplex").debug
    -- Simulate a tableau T with one artificial column (col 2), one real col (col 1), RHS at col 3
    local T = {
      { 0, 1, 5 }, -- row1 has art basic
      { 1, 0, 3 }, -- row2 real basic
      { 0, 0, 0 }  -- cost row unused here
    }
    local basis = { 2, 1 }
    local art_cols = { 2 }
    local T2, new_basis = dbg.purge_artificial(T, basis, art_cols)
    -- After purge, only real col remains (mapped to new_j=1), RHS at 2
    assert.are.same({ 1, 1 }, new_basis) -- both rows now basics in col1
    -- New tableau dimensions: 2 rows × (1 real + 1 RHS) = 2×2
    assert.are.equal(2, #T2[1])
    assert.are.equal(2, #T2[2])
  end)

  it("purge_artificial falls back to col1 when artificial has no real pivots", function()
    local dbg           = require("simplex").debug
    -- Tableau: only artificial basic in row1; real col (1) is all zeros
    local T             = {
      { 0, 1, 5 }, -- row1: only art at col2
      { 0, 0, 3 }, -- row2: also zeros in real col
      { 0, 0, 0 },
    }
    local basis         = { 2, 2 } -- both rows “basic” in artificial col
    local art_cols      = { 2 }
    local T2, new_basis = dbg.purge_artificial(T, basis, art_cols)
    -- Both should end up in col1 after fallback
    assert.are.same({ 1, 1 }, new_basis)
    -- New tableau has 1 real var + RHS
    assert.are.equal(2, #T2[1])
    assert.are.equal(2, #T2[2])
  end)


  it("reset_phase2_objective rebuilds reduced costs correctly", function()
    -- Subtracting basic‐cost × row from the bottom cost row for nonzero basic costs.
    local dbg = require("simplex").debug
    -- Tableau: two constraint rows + cost-row placeholder
    -- decision vars in cols 1–2, RHS in col 3
    local T = {
      { 1, 0, 4 },
      { 0, 1, 3 },
      { 0, 0, 0 },
    }
    local costs = { 3, 5 }
    local basis = { 1, 2 }
    dbg.reset_phase2_objective(T, costs, basis)
    -- Reduced costs: c_j – (c_b1*T[1][j] + c_b2*T[2][j])
    -- j=1: 3 – (3*1 + 5*0) = 0
    assert.are.equal(0, T[3][1])
    -- j=2: 5 – (3*0 + 5*1) = 0
    assert.are.equal(0, T[3][2])
    -- RHS: 0 – (3*4 + 5*3) = –27
    assert.are.equal(-27, T[3][3])
  end)

  it("init_phase2 then simplex_core reaches optimal for simple LP", function()
    -- end‐to‐end of Phase II init and a normal call to debug.simplex_core without cycling or
    -- unboundedness.
    local simplex              = require("simplex")
    local dbg                  = simplex.debug
    -- Build a tiny Phase I tableau manually:
    -- A = [1], b = [2], costs = {1}, so one slack reused, no art.
    local A, b, costs          = { { 1 } }, { 2 }, { 1 }
    local T1, basis1, art_cols = dbg.build_phase1(A, b, costs)
    local T2, basis2           = dbg.init_phase2(T1, basis1, art_cols, costs)
    -- Phase II cost row should now reflect c – c_b·row
    -- Then run simplex_core to optimal
    local status, Tf, basisf   = dbg.simplex_core(T2, basis2)
    assert.are.equal(simplex.Status.OPTIMAL, status)
    -- Basis should be {1}
    assert.are.same({ 1 }, basisf)
  end)
end)

describe("simplex.solve edge cases and robustness", function()
  local simplex = require "simplex"

  it("errors on invalid inputs", function()
    -- empty A matrix
    assert.has_error(function() simplex.solve({}, {}, {}) end)
    -- mismatched b length
    assert.has_error(function() simplex.solve({ { 1, 2 } }, { 1, 2 }, { 1, 2 }) end)
    -- mismatched costs length
    assert.has_error(function() simplex.solve({ { 1, 2 } }, { 3 }, { 1 }) end)
    -- non-table A
    assert.has_error(function() simplex.solve("not_a_matrix", { 1 }, { 1 }) end)
  end)

  it("solves trivial 1x1 LP", function()
    local status, x, obj = simplex.solve({ { 1 } }, { 1 }, { 1 })
    assert.are.equal(simplex.Status.OPTIMAL, status)
    assert_nearly_equal(x[1], 1)
    assert_nearly_equal(obj, 1)
  end)

  it("detects unbounded LP", function()
    -- minimize -x subject to 0·x = 0, x ≥ 0 ⇒ unbounded
    local status, x, obj = simplex.solve({ { 0 } }, { 0 }, { -1 })
    assert.are.equal(simplex.Status.UNBOUNDED, status)
    assert.is_nil(x)
    assert.is_nil(obj)
  end)

  it("detects infeasible LP", function()
    -- x = -1 with x ≥ 0 ⇒ infeasible
    local status = simplex.solve({ { 1 } }, { -1 }, { 1 })
    assert.are.equal(simplex.Status.INFEASIBLE, status)
  end)

  it("solves a unique-optimum LP", function()
    -- minimize x₁ + 2x₂ subject to x₁ + x₂ = 4
    local status, x, obj = simplex.solve({ { 1, 1 } }, { 4 }, { 1, 2 })
    assert.are.equal(simplex.Status.OPTIMAL, status)
    assert_nearly_equal(x[1], 4)
    assert_nearly_equal(x[2], 0)
    assert_nearly_equal(obj, 4)
  end)

  it("handles degenerate case with zero costs", function()
    -- minimize 0 subject to x₁ = 5, x₂ = 10
    local status, x, obj = simplex.solve({ { 1, 0 }, { 0, 1 } }, { 5, 10 }, { 0, 0 })
    assert.are.equal(simplex.Status.OPTIMAL, status)
    assert_nearly_equal(x[1], 5)
    assert_nearly_equal(x[2], 10)
    assert_nearly_equal(obj, 0)
  end)
end)

describe("simplex.solve more edge cases", function()
  local simplex = require "simplex"

  it("is infeasible when 0·x = b>0", function()
    -- No combination of x ≥ 0 can satisfy 0·x = 1
    local status = simplex.solve({ { 0, 0 } }, { 1 }, { 1, 1 })
    assert.are.equal(simplex.Status.INFEASIBLE, status)
  end)

  it("solves trivial 0·x = 0 with positive cost", function()
    -- Minimize 5x subject to 0·x = 0 => x = 0, obj = 0
    local status, x, obj = simplex.solve({ { 0 } }, { 0 }, { 5 })
    assert.are.equal(simplex.Status.OPTIMAL, status)
    assert_nearly_equal(x[1], 0)
    assert_nearly_equal(obj, 0)
  end)

  it("is infeasible on inconsistent duplicate constraints", function()
    -- x₁ + x₂ = 1 and x₁ + x₂ = 2 cannot both hold
    local status = simplex.solve({ { 1, 1 }, { 1, 1 } }, { 1, 2 }, { 1, 1 })
    assert.are.equal(simplex.Status.INFEASIBLE, status)
  end)

  it("correctly flips rows with negative RHS", function()
    -- -x₁ + x₂ = -1  (b<0) is flipped to x₁ - x₂ = 1
    local status, x, obj = simplex.solve({ { -1, 1 } }, { -1 }, { -1, 1 })
    assert.are.equal(simplex.Status.OPTIMAL, status)
    -- After flip: minimize -x₁ + x₂ subject to x₁ - x₂ = 1
    -- Optimal at x₁=1, x₂=0 ⇒ obj = -1
    assert_nearly_equal(x[1], 1)
    assert_nearly_equal(x[2], 0)
    assert_nearly_equal(obj, -1)
  end)
end)

describe("simplex.debug helper functions", function()
  local dbg = require("simplex").debug

  it("zero_small zeros entries below tolerance", function()
    local T = {
      { 1e-7, -1e-7 },
      { 1e-8, -1e-8 },
    }
    dbg.zero_small(T)
    for i = 1, 2 do
      for j = 1, 2 do
        assert.are.equal(0, T[i][j])
      end
    end
  end)

  it("choose_entering picks the most negative reduced-cost", function()
    -- single-row tableau interpreted as cost row
    local T = { { 10, -0.1, -2, 5 } }
    -- expect column 3 (value -2) chosen
    assert.are.equal(3, dbg.choose_entering(T))
  end)

  it("choose_leaving picks minimal positive ratio and ties by row index", function()
    -- T has 2 constraint rows then cost row
    local T = {
      { 2, 5 }, -- ratio = 2.5
      { 3, 6 }, -- ratio = 2
      {}        -- cost row, ignored by choose_leaving
    }
    -- entering column is 1
    assert.are.equal(2, dbg.choose_leaving(T, 1))
  end)
end)

describe("simplex_core cycle detection", function()
  it("returns INFEASIBLE when a basis repeats (cycling)", function()
    local simplex                  = require "simplex"
    local dbg                      = simplex.debug

    -- A 2-constraint tableau (m=2) with 2 decision cols + RHS (n+1=3).
    -- cost row has two equal negative reduced costs ⇒ entering = col 1.
    local T_cycle                  = {
      { 0,  1,  0 }, -- row 1
      { 1,  0,  0 }, -- row 2
      { -1, -1, 0 }, -- cost row
    }

    -- Initial basis picks col2 for row1 and col1 for row2.
    -- After pivoting on (row2,col1), basis remains {2,1}, so we cycle.
    local basis                    = { 2, 1 }

    local status, T_out, basis_out = dbg.simplex_core(T_cycle, basis)

    -- Should detect the repeated basis and bail out as INFEASIBLE.
    assert.are.equal(simplex.Status.INFEASIBLE, status)

    -- The basis remains the same, confirming the cycle.
    assert.are.same({ 2, 1 }, basis_out)
  end)
end)

it("solve handles the x₁=0,x₂=0 LP without cycling", function()
  local simplex = require "simplex"
  local status, x, obj = simplex.solve(
    { { 0, 1 }, { 1, 0 } }, -- A
    { 0, 0 },               -- b
    { 1, 1 }                -- costs
  )
  assert.are.equal(simplex.Status.OPTIMAL, status)
  assert.are.same({ 0, 0 }, x)
  assert.is_true(math.abs(obj - 0) < TOLERANCE)
end)

describe("3-D Klee-Minty cube (degenerate stress test)", function()
  local simplex = require "simplex"

  it("solves the 3-D worst-case LP in finite pivots", function()
    -- maximize x1 + 2x2 + 4x3  ⇒  minimize –(x1 + 2x2 + 4x3)
    -- subject to
    --   x1               ≤ 5
    --   4x1 + x2         ≤ 25
    --   16x1 + 4x2 + x3  ≤ 125
    --
    -- Only pass the “core” A, b, and costs for decision vars:
    local A              = {
      { 1,  0, 0 }, -- x1 ≤  5
      { 4,  1, 0 }, -- 4x1 + x2 ≤ 25
      { 16, 4, 1 }, -- 16x1 + 4x2 + x3 ≤ 125
    }
    local b              = { 5, 25, 125 }
    -- negative costs => maximization
    local costs          = { -1, -2, -4 }

    local status, x, obj = simplex.solve(A, b, costs)
    assert.are.equal(simplex.Status.OPTIMAL, status)

    -- Optimal decision vars all = 5
    assert_nearly_equal(x[1], 5)
    assert_nearly_equal(x[2], 5)
    assert_nearly_equal(x[3], 25)

    -- Slack variables (added internally) should be zero
    for i = 4, #x do
      assert_nearly_equal(x[i], 0)
    end

    -- Because we minimized –(x1+2x2+4x3), obj = –115
    assert_nearly_equal(obj, -115)
  end)
end)
