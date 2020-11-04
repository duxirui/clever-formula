package org.clever.calc;

/**
 * Description： 公式计算类
 * User: 沐彦
 * Date: Created in 2020-11-2 13:09
 * Version: 1.0.0
 * Modified By:
 */
public class Formula extends FormulaParser {

    private String formula;

    public Formula(String formula) {
        this.formula = formula;
    }

    public Formula(String formula, Variable... variables) {
        this(formula);
        if (null != variables) {
            for (Variable variable : variables) {
                this.variableMaps.put(variable.getVar(), variable.getVal());
            }
        }

    }

    public Object calculation() {
        return calc(formula);
    }

}
