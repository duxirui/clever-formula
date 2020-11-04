package org.clever.exception;

/**
 * Description： 公式解析异常
 * User: 沐彦
 * Date: Created in 2020/10/6 08:59
 * Version: 1.0.0
 * Modified By:
 */
public class FormulaParserException extends RuntimeException {

    public FormulaParserException(String msg) {
        super(msg);
    }

    public FormulaParserException() {
        super();
    }

}
