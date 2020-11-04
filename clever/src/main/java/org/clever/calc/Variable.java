package org.clever.calc;

/**
 * Description： 公式计算变量类
 * User: 沐彦
 * Date: Created in 2020-11-2 13:09
 * Version: 1.0.0
 * Modified By:
 */
public class Variable {

    public Variable() {

    }

    public Variable(String var, Object val) {
        this.var = var;
        this.val = val;
    }

    /**
     * 变量名
     */
    private String var;

    /**
     * 变量值
     */
    private Object val;

    public String getVar() {
        return this.var;
    }

    public String getVal() {
        return String.valueOf(this.val);
    }

}
