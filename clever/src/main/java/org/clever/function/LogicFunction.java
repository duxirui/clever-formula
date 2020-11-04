//package org.clever.function;
//
//import com.etransfar.tims3.common.exception.TimsException;
//import com.etransfar.tims3.s2.auth.formula.utils.FunctionUtil;
//import lombok.AllArgsConstructor;
//import lombok.Getter;
//import org.mariuszgromada.math.mxparser.Expression;
//
///**
// * Description： 逻辑函数
// * User: 沐彦
// * Date: Created in 2020/9/25 10:08
// * Version: 1.0.0
// * Modified By:
// */
//public class LogicFunction {
//
//    /**
//     * 如果所有参数都为真，AND函数返回布尔值true，否则返回布尔值 false
//     *
//     * @param cs 参数列表
//     * @return 计算结果
//     */
//    private boolean and(Object[] cs) {
//        if (null == cs || cs.length <= 0) {
//            throw new TimsException("逻辑公式and条件参数非法");
//        }
//        Expression e = FunctionUtil.newExpression("and(%s)", FunctionUtil.array2String(cs));
//        return FunctionUtil.double2Bool(e.calculate());
//    }
//
//    /**
//     * 如果任意参数为真，OR 函数返回布尔值true；如果所有参数为假，返回布尔值false。
//     *
//     * @param cs 参数列表
//     * @return 计算结果
//     */
//    private boolean or(Object[] cs) {
//        if (null == cs || cs.length <= 0) {
//            throw new TimsException("逻辑公式or条件参数非法");
//        }
//        Expression e = FunctionUtil.newExpression("or(%s)", FunctionUtil.array2String(cs));
//        return FunctionUtil.double2Bool(e.calculate());
//    }
//
//    /**
//     * 返回布尔值false
//     *
//     * @return false
//     */
//    private boolean getFalse() {
//        return false;
//    }
//
//    /**
//     * 返回布尔值true
//     *
//     * @return true
//     */
//    private boolean getTrue() {
//        return true;
//    }
//
//    /**
//     * 判断一个条件能否满足；如果满足返回一个值，如果不满足则返回另外一个值
//     *
//     * @param c      判断条件
//     * @param trueV  结果为true返回的结果
//     * @param falseV 结果为false返回的结果
//     * @return 返回结果
//     */
//    private Object inCase(Object c, Object trueV, Object falseV) {
//        if (null == c || null == trueV || null == falseV) {
//            throw new TimsException("逻辑公式if判断参数非法");
//        }
//        if (and(new Object[]{c})) {
//            return trueV;
//        }
//        return falseV;
//    }
//
//    /**
//     * 检查是否满足一个或多个条件，且返回符合第一个TRUE条件的值，IFS可以取代多个嵌套IF语句。
//     *
//     * @param sics 判断条件
//     * @return 执行结果
//     */
//    private Object inCases(SubInCase[] sics) {
//        if (null == sics || sics.length <= 0) {
//            throw new TimsException("逻辑公式ifs判断参数非法");
//        }
//        for (SubInCase sic : sics) {
//            if (and(new String[]{sic.getC()})) {
//                return sic.getTrueV();
//            }
//        }
//        throw new TimsException("无计算结果");
//    }
//
//    /**
//     * 返回与指定表达式相反的布尔值。
//     *
//     * @param c 表达式
//     * @return 返回结果
//     */
//    private boolean not(Object c) {
//        if (c instanceof Boolean) {
//            return !((Boolean) c);
//        } else if (c instanceof String || c instanceof Integer || c instanceof Long || c instanceof Float || c instanceof Double || c instanceof Character || c instanceof Byte || c instanceof String) {
//            if (c instanceof String) {
//                if ("true".equalsIgnoreCase(c.toString().trim())) {
//                    c = "1=1";
//                } else if ("false".equalsIgnoreCase(c.toString().trim())) {
//                    c = "1=0";
//                } else {
//                    throw new TimsException("逻辑公式not判断参数非法");
//                }
//            }
//            Expression e = FunctionUtil.newExpression("not(%s)", String.valueOf(c));
//            return FunctionUtil.double2Bool(e.calculate());
//        }
//        throw new TimsException("逻辑公式not判断参数非法");
//    }
//
//    /**
//     * 返回所有参数的异或值
//     *
//     * @param cs 条件数组
//     * @return 判断条件
//     */
//    private boolean xor(Object[] cs) {
//        if (null == cs || cs.length <= 0) {
//            throw new TimsException("逻辑公式xor判断参数非法");
//        }
//        if (cs.length < 2) {
//            return false;
//        }
//        String s = null;
//        for (Object oc : cs) {
//            String c = String.valueOf(oc);
//            String temp;
//            if (c.contains("==") || c.contains("=") || c.contains("<") || c.contains("<=")
//                    || c.contains(">") || c.contains(">=") || c.contains("<>") || c.contains("!=")) { // 疑似表达式
//                try {
//                    temp = String.valueOf(and(new String[]{c}));
//                } catch (TimsException te) {
//                    temp = c;
//                } catch (Exception e) {
//                    throw new TimsException("逻辑公式xor处理失败");
//                }
//            } else {
//                temp = c;
//            }
//            if (null == s) {
//                s = temp;
//            }
//            if (!s.equals(temp)) {
//                return true;
//            }
//        }
//        return false;
//    }
//
//    @Getter
//    @AllArgsConstructor
//    public static class SubInCase {
//        private String c;
//        private Object trueV;
//    }
//
//}
