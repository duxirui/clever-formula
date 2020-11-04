package org.clever.utils;

import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.util.Calendar;
import java.util.Date;

/**
 * Description： 处理数组的工具类
 * User: 沐彦
 * Date: Created in 2020/9/25 10:11
 * Version: 1.0.0
 * Modified By:
 */
public class FormulaUtil {

    /**
     * 将array转换层字符串
     *
     * @param vs 对象数组
     * @return
     */
    public static String array2String(Object[] vs) {
        if (null == vs || vs.length <= 0) return "";
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < vs.length; i++) {
            if (0 < i) {
                sb.append(", ");
            }
            if (vs[i] instanceof Boolean) {
                if ((Boolean) vs[i]) {
                    sb.append("1=1");
                } else {
                    sb.append("0=1");
                }
            } else if (vs[i] instanceof String) {
                if ("false".equalsIgnoreCase(vs[i].toString().trim())) {
                    sb.append("0=1");
                } else if ("true".equals(vs[i].toString().trim())) {
                    sb.append("1=1");
                } else {
                    sb.append(vs[i]);
                }
            } else {
                sb.append(vs[i]);
            }

        }
        return sb.toString();
    }

    /**
     * double类型转换为boolean类型
     *
     * @param v 数值
     * @return 布尔类型
     */
    public static boolean double2Bool(double v) {
        if (new BigDecimal(v).equals(BigDecimal.ZERO)) {
            return false;
        }
        return true;
    }

    /**
     * 截取小数的整数部分
     *
     * @param d 小数
     * @return 整数
     */
    public static int myRound(double d) {
        String s = String.valueOf(d);
        s = s.substring(0, s.contains(".") ? s.lastIndexOf(".") : s.length());
        return Integer.parseInt(s);
    }

    /**
     * 将数值转换为汉字
     *
     * @param d 数字
     * @param b false:小写、true：大写
     * @return 汉字
     */
    public static String toChinese(double d, boolean b) {
        String[] s1 = null;
        String[] s2 = null;
        if (b) {
            s1 = new String[]{"零", "壹", "贰", "叁", "肆", "伍", "陆", "柒", "捌", "玖"};
            s2 = new String[]{"拾", "佰", "仟", "万", "拾", "佰", "仟", "亿", "拾", "佰", "仟"};
        } else {
            s1 = new String[]{"零", "一", "二", "三", "四", "五", "六", "七", "八", "九"};
            s2 = new String[]{"十", "百", "千", "万", "十", "百", "千", "亿", "十", "百", "千"};
        }
        String result = "";
        String s = new BigDecimal(d).toString();
        String main;
        String point = null;
        if (s.indexOf(".") > 0 && Integer.parseInt(s.substring(s.indexOf(".") + 1)) > 0) { // 浮点数
            point = s.substring(s.indexOf(".") + 1);
            main = s.substring(0, s.indexOf("."));
        } else {
            main = String.valueOf(new Double(d).longValue());
        }
        int n = main.length();
        for (int i = 0; i < n; i++) {
            int num = main.charAt(i) - '0';
            if (i != n - 1 && num != 0) {
                result += s1[num] + s2[n - 2 - i];
            } else {
                result += s1[num];
            }
        }
        if (null != point) {
            result += "点";
            result += toPoint(point, s1);
        }
        return result;
    }

    /**
     * 处理小数点部分
     *
     * @param str 自渡船
     * @param s   汉字数组
     * @return 返回值
     */
    private static String toPoint(String str, String[] s) {
        StringBuffer sb = new StringBuffer();
        for (char c : str.toCharArray()) {
            sb.append(s[Integer.parseInt(String.valueOf(c))]);
        }
        return sb.toString();
    }

    /**
     * 数字格式化
     *
     * @param d 数字
     * @param f 格式化
     * @return 格式化后的数据
     */
    public static String numFormat(Double d, String f) {
        if ("[Num1]".equals(f)) {
            return FormulaUtil.toChinese(d, false);
        } else if ("[Num2]".equals(f)) {
            return FormulaUtil.toChinese(d, true);
        } else if (f.endsWith("[Num0]")) {
            return f.replace("[Num0]", new BigDecimal(d).toString());
        }
        DecimalFormat format = new DecimalFormat(f);
        return format.format(d);
    }

//    /**
//     * 时间格式函数
//     *
//     * @param d
//     * @param f
//     * @return
//     */
//    public static String dateFormat(Date d, String f) {
//        Calendar c = Calendar.getInstance();
//        c.setTime(d);
//        if ("E".equals(f)) {
//            return String.valueOf(c.get(Calendar.DAY_OF_WEEK) - 1);
//        } else if ("EE".equals(f)) {
//            String[] s = {"日", "一", "二", "三", "四", "五", "六"};
//            return s[c.get(Calendar.DAY_OF_WEEK) - 1];
//        }
//        return DateUtil.date2String(d, f);
//    }

    /**
     * 判断是否为数字
     *
     * @param str 字符串
     * @return
     */
    public static boolean isNumber(String str) {
        if (null == str || "".equals(str)) {
            return false;
        }
        str = str.trim();
        if (str.startsWith("-") || str.startsWith("+")) {
            str = str.substring(1);
        }
        boolean point = false;
        for (int index = 0; index < str.length(); index++) {
            if (!Character.isDigit(str.charAt(index))) {
                if (!point && ".".equals(String.valueOf(str.charAt(index)))) {
                    point = true;
                } else {
                    return false;
                }
            }
        }
        return true;
    }

    /**
     * 返回第2个参数的最新boolean值
     *
     * @param o 浮点数
     * @return boolean类型
     */
    public static boolean toBool(Object o) {
        if (0 == FormulaUtil.myRound((Double) o)) {
            return false;
        } else {
            return true;
        }
    }

}
