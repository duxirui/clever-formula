package org.clever.function;

import org.clever.calc.Formula;
import org.clever.exception.FormulaParserException;
import org.clever.utils.FormulaUtil;

/**
 * Description： 逻辑函数
 * User: 沐彦
 * Date: Created in 2020/9/25 10:08
 * Version: 1.0.0
 * Modified By:
 */
public class LogicFunction {

    /**
     * 如果所有参数都为真，AND函数返回布尔值true，否则返回布尔值 false
     *
     * @param cs 参数列表
     * @return 计算结果
     */
    private boolean and(Object[] cs) {
        if (null == cs || cs.length <= 0) {
            throw new FormulaParserException("Logic formula and condition parameters are illegal");
        }
        for (Object o : cs) {
            if (o instanceof Double || o instanceof Long || o instanceof Float || o instanceof Integer) {
                Formula formula = new Formula(o + " == 0");
                Boolean r = (Boolean) formula.calculation();
                if (!r) {
                    return false;
                }
            } else if (o instanceof Boolean) {
                if (!(Boolean) o) return false;
            } else {
                if(Boolean.TRUE.toString().equalsIgnoreCase(String.valueOf(o))) {
                    continue;
                }
                Formula formula = new Formula(String.valueOf(o));
                Object r = formula.calculation();
                if (r instanceof Double || r instanceof Long || r instanceof Float || r instanceof Integer) {
                    if (!FormulaUtil.double2Bool(((Number) r).doubleValue())) return false;
                } else if (r instanceof Boolean) {
                    if (!(Boolean) r) return false;
                } else {
                    return false;
                }
            }
        }
        return true;
    }

    /**
     * 如果任意参数为真，OR 函数返回布尔值true；如果所有参数为假，返回布尔值false。
     *
     * @param cs 参数列表
     * @return 计算结果
     */
    private boolean or(Object[] cs) {
        if (null == cs || cs.length <= 0) {
            throw new FormulaParserException("Logic formula or condition parameters are illegal");
        }
        for (Object o : cs) {
            if (o instanceof Double || o instanceof Long || o instanceof Float || o instanceof Integer) {
                Formula formula = new Formula(o + " == 0");
                Boolean r = (Boolean) formula.calculation();
                if (r) {
                    return true;
                }
            } else if (o instanceof Boolean) {
                if ((Boolean) o) return true;
            } else {
                if(Boolean.FALSE.toString().equalsIgnoreCase(String.valueOf(o))) {
                    continue;
                }
                Formula formula = new Formula(String.valueOf(o));
                Object r = formula.calculation();
                if (r instanceof Double || r instanceof Long || r instanceof Float || r instanceof Integer) {
                    if (FormulaUtil.double2Bool(((Number) r).doubleValue())) return true;
                } else if (r instanceof Boolean) {
                    if ((Boolean) r) return true;
                }
            }
        }
        return false;
    }

    /**
     * 返回布尔值false
     *
     * @return false
     */
    private boolean getFalse() {
        return false;
    }

    /**
     * 返回布尔值true
     *
     * @return true
     */
    private boolean getTrue() {
        return true;
    }

    /**
     * 判断一个条件能否满足；如果满足返回一个值，如果不满足则返回另外一个值
     *
     * @param c      判断条件
     * @param trueV  结果为true返回的结果
     * @param falseV 结果为false返回的结果
     * @return 返回结果
     */
    private Object inCase(Object c, Object trueV, Object falseV) {
        if (null == c || null == trueV || null == falseV) {
            throw new FormulaParserException("Logic formula if judgment parameter is illegal");
        }
        if (and(new Object[]{c})) {
            return trueV;
        }
        return falseV;
    }

    /**
     * 返回与指定表达式相反的布尔值。
     *
     * @param c 表达式
     * @return 返回结果
     */
    private boolean not(Object c) {
        if (c instanceof Boolean) {
            return !((Boolean) c);
        } else if (c instanceof String || c instanceof Integer || c instanceof Long || c instanceof Float || c instanceof Double || c instanceof Character || c instanceof Byte || c instanceof String) {
            if (c instanceof String) {
                if (Boolean.TRUE.toString().equalsIgnoreCase(c.toString().trim())) {
                    c = "1=1";
                } else if (Boolean.FALSE.toString().equalsIgnoreCase(c.toString().trim())) {
                    c = "1=0";
                } else {
                    throw new FormulaParserException("The logic formula does not judge that the parameter is illegal");
                }
            }
            Formula formula = new Formula(c.toString());
            Boolean r = (Boolean) formula.calculation();
            return !r;
        }
        throw new FormulaParserException("The logic formula does not judge that the parameter is illegal");
    }

    /**
     * 返回所有参数的异或值
     *
     * @param cs 条件数组
     * @return 判断条件
     */
    private boolean xor(Object[] cs) {
        if (null == cs || cs.length <= 0) {
            throw new FormulaParserException("The logic formula xor judges that the parameter is illegal");
        }
        if (cs.length < 2) {
            return false;
        }
        String s = null;
        for (Object oc : cs) {
            String c = String.valueOf(oc);
            String temp;
            if (c.contains("==") || c.contains("=") || c.contains("<") || c.contains("<=")
                    || c.contains(">") || c.contains(">=") || c.contains("<>") || c.contains("!=")) { // 疑似表达式
                try {
                    temp = String.valueOf(and(new String[]{c}));
                } catch (FormulaParserException te) {
                    temp = c;
                } catch (Exception e) {
                    throw new FormulaParserException("Logical formula xor processing failed");
                }
            } else {
                temp = c;
            }
            if (null == s) {
                s = temp;
            }
            if (!s.equals(temp)) {
                return true;
            }
        }
        return false;
    }

}
