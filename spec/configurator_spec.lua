local LP = require("configurator")
local simplex = require("simplex")

-- Copy from simplex_spec
local tol = 2 ^ -20
local function nearly_equal(a, b)
  if math.abs(a - b) <= tol then return true end
  local max_ab = math.max(math.abs(a), math.abs(b))
  return max_ab > 0 and math.abs(a - b) / max_ab <= tol
end

describe("Configurator oil example test", function()
  it("finds the expected optimal solution", function()
    local lp = LP.new_lp_problem()

    LP.add_recipe(lp, "heavyToLight", {
      { "heavy", -40 }, { "light", 30 }, { "water", -30 },
    })

    LP.add_recipe(lp, "lightToPGas", {
      { "light", -30 }, { "pGas", 20 }, { "water", -30 },
    })

    LP.add_recipe(lp, "basicOil", {
      { "heavy", 30 }, { "light", 30 }, { "pGas", 40 }, { "crude", -100 },
    })

    LP.add_recipe(lp, "advOil", {
      { "heavy", 10 }, { "light", 45 }, { "pGas", 55 }, { "water", -50 }, { "crude", -100 },
    })

    LP.add_recipe(lp, "rawWater", { { "water", 1 } })
    LP.add_recipe(lp, "rawCrude", { { "crude", 1 } })

    LP.constrain_material(lp, "heavy", LP.CompareType.GREATER_OR_EQUAL, 10)
    LP.constrain_material(lp, "light", LP.CompareType.GREATER_OR_EQUAL, 0)
    LP.constrain_material(lp, "pGas", LP.CompareType.GREATER_OR_EQUAL, 45)
    LP.constrain_material(lp, "water", LP.CompareType.GREATER_OR_EQUAL, 0)
    LP.constrain_material(lp, "crude", LP.CompareType.GREATER_OR_EQUAL, 0)

    LP.optimize(lp, "rawCrude", 1, LP.ObjectiveDirection.MINIMIZE)

    local status, solution, objective = LP.finalize(lp)

    assert.are.equal(simplex.Status.OPTIMAL, status)
    assert.is_true(nearly_equal(objective, 58.9744), "Objective mismatch")

    local expected = {
      heavyToLight = 0,
      lightToPGas = 0.782051,
      basicOil = 0.205128,
      advOil = 0.384615,
      rawWater = 42.6923,
      rawCrude = 58.9744,
    }

    for name, expected_val in pairs(expected) do
      assert.is_true(
        nearly_equal(solution[name], expected_val),
        string.format("Mismatch on %s: got %g, expected %g", name, solution[name], expected_val)
      )
    end
  end)
end)
