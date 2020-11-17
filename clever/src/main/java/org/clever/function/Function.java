package org.clever.function;

import org.clever.exception.FunctionFormulaCalcException;

import java.lang.reflect.Method;

/**
 * Description： 函数注册枚举
 * <p>
 * User: 沐彦
 * Date: Created in 2020/9/23 14:07
 * Version: 1.0.0
 * Modified By:
 */
public enum Function {

    /**
     * 数学函数
     */
    ABS("abs", MathFunction.class),
    AVERAGE("average", MathFunction.class),
    CEILING("ceiling", MathFunction.class),
    COUNT("count", MathFunction.class),
    COUNTIF("countif", MathFunction.class),
    FIXED("fixed", MathFunction.class),
    FLOOR("floor", MathFunction.class),
    INT("toTnt", MathFunction.class),
    LARGE("large", MathFunction.class),
    LOG("log", MathFunction.class),
    MAX("max", MathFunction.class),
    MIN("min", MathFunction.class),
    MOD("mod", MathFunction.class),
    POWER("power", MathFunction.class),
    PRODUCT("product", MathFunction.class),
    RAND("rand", MathFunction.class),
    ROUND("round", MathFunction.class),
    SMALL("small", MathFunction.class),
    SQRT("sqrt", MathFunction.class),
    SUM("sum", MathFunction.class),
    SUMPRODUCT("sumproduct", MathFunction.class),

    /**
     * 逻辑函数
     */
    AND("and", LogicFunction.class),
    FALSE("getFalse", LogicFunction.class),
    IF("inCase", LogicFunction.class),
    NOT("not", LogicFunction.class),
    OR("or", LogicFunction.class),
    TRUE("getTrue", LogicFunction.class),
    XOR("xor", LogicFunction.class);
//
//    /**
//     * 时间函数
//     */
//    DATE("date", "将时间戳转换为日期对象", DateFunction.class, OperatorType.DATA),
//    DATEDELTA("datedelta", "将指定日期加/减指定天数 ", DateFunction.class, OperatorType.DATA),
//    DATEDIF("datedif", "", DateFunction.class, OperatorType.DATA),
//    DAY("day", "", DateFunction.class, OperatorType.DATA),
//    DAYS("days", "", DateFunction.class, OperatorType.DATA),
//    DAYS360("days360", "", DateFunction.class, OperatorType.DATA),
//    HOUR("hour", "", DateFunction.class, OperatorType.DATA),
//    MOUTH("mouth", "", DateFunction.class, OperatorType.DATA),
//    MINUTE("minute", "", DateFunction.class, OperatorType.DATA),
//    TODAY("today", "", DateFunction.class, OperatorType.DATA),
//    SYSTIME("systime", "", DateFunction.class, OperatorType.DATA),
//    TIME("time", "", DateFunction.class, OperatorType.DATA),
//    TIMESTAMP("timestamp", "", DateFunction.class, OperatorType.DATA),
//    WEEKNUM("weeknum", "", DateFunction.class, OperatorType.DATA),
//    YEAR("year", "", DateFunction.class, OperatorType.DATA),
//
//    /**
//     * 文本函数
//     */
//    CONCATENATE("splice", "", TextFunction.class, OperatorType.TEXT),
//    CHAR("toChar", "", TextFunction.class, OperatorType.TEXT),
//    EXACT("equals", "", TextFunction.class, OperatorType.TEXT),
//    LEFT("left", "", TextFunction.class, OperatorType.TEXT),
//    LEN("len", "", TextFunction.class, OperatorType.TEXT),
//    LOWER("lower", "", TextFunction.class, OperatorType.TEXT),
//    REPLACE("replace", "", TextFunction.class, OperatorType.TEXT),
//    REPT("rept", "", TextFunction.class, OperatorType.TEXT),
//    SEARCH("search", "", TextFunction.class, OperatorType.TEXT),
//    RIGHT("right", "", TextFunction.class, OperatorType.TEXT),
//    SPLIT("split", "", TextFunction.class, OperatorType.TEXT),
//    TRIM("trim", "", TextFunction.class, OperatorType.TEXT),
//    UPPER("upper", "", TextFunction.class, OperatorType.TEXT),
//    MID("mid", "", TextFunction.class, OperatorType.TEXT),
//    TEXT("text", "", TextFunction.class, OperatorType.TEXT),
//    VALUE("value", "", TextFunction.class, OperatorType.TEXT),
//    UNION("union", "", TextFunction.class, OperatorType.TEXT);


    private String methodName;

    private String caption;

    private Class functionClass;

    Function(String methodName, Class functionClass) {
        this.methodName = methodName;
        this.functionClass = functionClass;
    }

    /**
     * 计算
     *
     * @param params 参数
     * @return 计算结果
     */
    public Object calc(Object[] params) {
        try {
            Method method = getMethod();
            method.setAccessible(true);
            if (params.length != method.getParameterCount()) {
                throw new FunctionFormulaCalcException("Dynamic execution calculation function parameters do not match!");
            }
            Class[] parameterTypesClass = method.getParameterTypes();
            Object[] paramObjs = new Object[params.length];
            for (int i = 0; i < parameterTypesClass.length; i++) {
                paramObjs[i] = params[i];
            }
            return method.invoke(this.functionClass.newInstance(), paramObjs);
        } catch (Exception e) {
            e.printStackTrace();
            throw new FunctionFormulaCalcException("Dynamic execution function " + this.toString() + " calculation failed");
        }
    }

    /**
     * 获取非数组参数个数
     *
     * @return 个数
     */
    public int getFixedParamNum() {
        Method method = getMethod();
        Class[] paramClasses = method.getParameterTypes();
        int r = paramClasses.length;
        for (Class clazz : paramClasses) {
            if (clazz.getTypeName().endsWith("[]")) {
                r--;
            }
        }
        return r;
    }

    /**
     * 获取参数格式
     *
     * @return 个数
     */
    public int getParamNum() {
        Method method = getMethod();
        return method.getParameterTypes().length;
    }

    /**
     * 获取函数枚举对象
     *
     * @param name 函数名
     * @return 函数枚举对象
     */
    public static Function get(String name) {
        if (null == name || "".equals(name)) {
            throw new FunctionFormulaCalcException("Function name cannot be empty");
        }
        for (Function function : Function.values()) {
            if (function.toString().equals(name)) {
                return function;
            }
        }
        throw new FunctionFormulaCalcException("Function " + name + " does not exist");
    }

    /**
     * 获取返回值类型
     *
     * @return 返回值类型
     */
    private Class getReturnType() {
        Method method = getMethod();
        return method.getReturnType();
    }

    /**
     * 查找对应的方法对象
     *
     * @return 方法对象
     */
    private Method getMethod() {
        Method[] methods = this.functionClass.getDeclaredMethods();
        for (Method method : methods) {
            if (this.methodName.equalsIgnoreCase(method.getName())) {
                return method;
            }
        }
        throw new FunctionFormulaCalcException("The formula method that needs to be calculated is not found");
    }

}
