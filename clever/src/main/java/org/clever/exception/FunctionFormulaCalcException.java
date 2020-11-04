package org.clever.exception;

/**
 * Description： 函数公式计算异常
 * User: 沐彦
 * Date: Created in 2020/10/6 08:59
 * Version: 1.0.0
 * Modified By:
 */
public class FunctionFormulaCalcException extends RuntimeException {

    public FunctionFormulaCalcException(String msg) {
        super(msg);
    }

    public FunctionFormulaCalcException() {
        super();
    }

}
