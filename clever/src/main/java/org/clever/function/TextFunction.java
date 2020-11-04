//package org.clever.function;
//
//import com.alibaba.fastjson.JSON;
//import com.etransfar.tims3.common.exception.TimsException;
//import com.etransfar.tims3.common.utils.DateUtil;
//import com.etransfar.tims3.s2.auth.formula.parser.FormulaParserException;
//import com.etransfar.tims3.s2.auth.formula.utils.FunctionUtil;
//import org.apache.commons.lang3.StringUtils;
//import org.apache.logging.log4j.util.Strings;
//
//import java.util.ArrayList;
//import java.util.Date;
//import java.util.List;
//
///**
// * Description： 文本函数
// * User: 杜涛
// * Date: Created in 2020/10/12 14:55
// * Version: 1.0.0
// * Modified By:
// */
//public class TextFunction {
//
//    /**
//     * 拼接多个文本字符串合并成一个字符串
//     *
//     * @param vs 参数
//     * @return 计算后的字符串
//     */
//    private String splice(Object[] vs) {
//        if (null == vs) {
//            throw new FormulaParserException("文本函数CONCATENATE参数非法");
//        }
//        StringBuilder s = new StringBuilder();
//        for (Object v : vs) {
//            if (v instanceof Double) {
//                String d = String.valueOf(v);
//                if (d.indexOf(".") < 0 || Integer.parseInt(d.substring(d.indexOf(".") + 1)) == 0) { // 整数
//                    s.append(((Double) v).longValue());
//                } else {
//                    s.append(v);
//                }
//            } else {
//                s.append(v);
//            }
//        }
//        return s.toString();
//    }
//
//    /**
//     * 返回计算机字符集数字代码所对应的字符
//     *
//     * @param v 数字
//     * @return 字符
//     */
//    private String toChar(Double v) {
//        if (null == v) {
//            throw new FormulaParserException("文本函数CHAR参数非法");
//        }
//        int iv = FunctionUtil.myRound(v);
//        char c = (char) iv;
//        return String.valueOf(c);
//    }
//
//    /**
//     * 比较两个字符串是否相等
//     *
//     * @param v1 字符串1
//     * @param v2 字符串1
//     * @return 结果
//     */
//    private boolean equals(String v1, String v2) {
//        return StringUtils.equals(v1, v2);
//    }
//
//    /**
//     * 从文本字符串第一个字符开始返回指定个数个数的字符
//     *
//     * @param v 字符串
//     * @param d 数字
//     * @return 字符串
//     */
//    private String left(String v, double d) {
//        if (null == v) {
//            throw new FormulaParserException("文本函数LEFT参数非法");
//        }
//        if (d < 0) {
//            return Strings.EMPTY;
//        } else if (d > v.length()) {
//            return v;
//        }
//        return v.substring(0, FunctionUtil.myRound(d));
//    }
//
//    /**
//     * 返回文本字符串中的字符个数
//     *
//     * @param v 字符串
//     * @return 字符个数
//     */
//    private int len(String v) {
//        if (null == v) {
//            throw new FormulaParserException("文本函数LEN参数非法");
//        }
//        return v.length();
//    }
//
//    /**
//     * 将文本字符串中所有大写字母转换成小写字母
//     *
//     * @param v 字符串
//     * @return 结果
//     */
//    private String lower(String v) {
//        if (null == v) {
//            throw new FormulaParserException("文本函数LOWER参数非法");
//        }
//        return v.toLowerCase();
//    }
//
//    /**
//     * 根据指定的字符数，将部分文本字符串替换为不同的文本字符串。
//     *
//     * @param oldText  要替换其部分字符的文本。
//     * @param startNum old_text中要替换为new_text的字符位置。
//     * @param endNum   old_text中希望使用new_text来进行替换的字符数。
//     * @param newText  将替换old_text中字符的文本。
//     * @return 计算结果
//     */
//    private String replace(String oldText, double startNum, double endNum, String newText) {
//        if (null == oldText || null == newText || startNum > endNum || startNum < 0 || endNum > oldText.length()) {
//            throw new FormulaParserException("文本函数REPLACE参数非法");
//        }
//        StringBuilder s = new StringBuilder();
//        s.append(oldText.substring(0, FunctionUtil.myRound(startNum) - 1));
//        s.append(newText);
//        s.append(oldText.substring(FunctionUtil.myRound(endNum) + 1));
//        return s.toString();
//    }
//
//    /**
//     * 将文本重复一定次数
//     *
//     * @param s 需要重复显示文本
//     * @param d 指定文本重复次数的正数
//     * @return 重复后的文本
//     */
//    private String rept(String s, double d) {
//        if (null == s || d <= 0) {
//            throw new TimsException("文本函数REPT参数非法");
//        }
//        StringBuilder sb = new StringBuilder();
//        for (int i = 1; i <= FunctionUtil.myRound(d); i++) {
//            sb.append(s);
//        }
//        return sb.toString();
//    }
//
//    /**
//     * 查询文本返回位置
//     *
//     * @param v1    检索内容
//     * @param v2    原内容
//     * @param start 其实位置
//     * @return 查询位置
//     */
//    private int search(String v1, String v2, double start) {
//        if (null == v1 || null == v2) {
//            throw new TimsException("文本函数SEARCH参数非法");
//        }
//        return v2.indexOf(v1, FunctionUtil.myRound(start)) + 1;
//    }
//
//    /**
//     * 返回文本值中最右边的字符
//     *
//     * @param v  字符串
//     * @param dd 个数
//     * @return 结果
//     */
//    private String right(String v, double dd) {
//        if (null == v) {
//            throw new FormulaParserException("文本函数RIGHT参数非法");
//        }
//        int d = FunctionUtil.myRound(dd);
//        if (d < 1) {
//            return v;
//        } else if (d > v.length()) {
//            return Strings.EMPTY;
//        }
//        return v.substring(v.length() - d);
//    }
//
//    /**
//     * 按指定字符串分割成数组
//     *
//     * @param v 字符串
//     * @param s 分隔符
//     * @return 字符串数组
//     */
//    private String[] split(String v, String s) {
//        if (null == v || null == s) {
//            throw new FormulaParserException("文本函数SPLIT参数非法");
//        }
//        return v.split(s);
//    }
//
//    /**
//     * 去除字符串内的空格和首位空格
//     *
//     * @param s 文本
//     * @return 处理后的文本
//     */
//    private String trim(String s) {
//        if (null == s) {
//            throw new FormulaParserException("文本函数SPLIT参数非法");
//        }
//        StringBuilder sb = new StringBuilder();
//        Boolean spaceFlg = null;
//        for (Character c : s.toCharArray()) {
//            if (null == spaceFlg) {
//                if (c.equals(' ')) {
//                    spaceFlg = false;
//                }
//            } else {
//                if (!spaceFlg && c.equals(' ')) {
//                    spaceFlg = true;
//                } else if (spaceFlg && !c.equals(' ')) {
//                    spaceFlg = null;
//                }
//            }
//            if (null == spaceFlg || !spaceFlg) {
//                sb.append(c);
//            }
//        }
//        return sb.toString().trim();
//    }
//
//    /**
//     * 将文本中的所有小写字母转换成大写字母
//     *
//     * @param s 文本
//     * @return 字符串
//     */
//    private String upper(String s) {
//        if (null == s) {
//            throw new FormulaParserException("文本函数UPPER参数非法");
//        }
//        return s.toUpperCase();
//    }
//
//    /**
//     * 返回文本字符串中从指定位置开始的特定数目的字符
//     *
//     * @param s     原文本
//     * @param start 起始位置
//     * @param num   取值个数
//     * @return 返回文本
//     */
//    private String mid(String s, double start, double num) {
//        if (null == s || start < 0 || num < 0 || (start + num) > s.length()) {
//            throw new FormulaParserException("文本函数MID参数非法");
//        }
//        int iStart = FunctionUtil.myRound(start);
//        int iNum = FunctionUtil.myRound(num);
//        iStart--;
//        return s.substring(iStart, (iStart + iNum));
//    }
//
//    /**
//     * 将其他对象转换为字符串
//     *
//     * @param os 对象
//     * @return 字符串
//     */
//    private String text(Object[] os) {
//        if (os == null || os.length != 2) {
//            throw new TimsException("文本函数TEXT参数非法");
//        }
//        if (1 == os.length) {
//            if (os[0] instanceof Date) {
//                return DateUtil.date2String((Date) os[0], DateUtil.DEFAULT_PATTERN);
//            }
//            if (os[0] instanceof Double) {
//                String d = String.valueOf(os[0]);
//                if (d.indexOf(".") < 0 || Integer.parseInt(d.substring(d.indexOf(".") + 1)) == 0) {
//                    return String.valueOf(FunctionUtil.myRound((Double) os[0]));
//                }
//            }
//        } else {
//            if (os[0] instanceof Date) {
//                return FunctionUtil.dateFormat((Date) os[0], String.valueOf(os[1]));
//            } else if (os[0] instanceof Double || os[0] instanceof Long || os[0] instanceof Integer || os[0] instanceof Float) {
//                return FunctionUtil.numFormat(Double.parseDouble(os[0].toString()), String.valueOf(os[1]));
//            }
//        }
//        return String.valueOf(os[0]);
//    }
//
//    /**
//     * 转换数值
//     *
//     * @param s 文本
//     * @return 数字
//     */
//    private double value(String s) {
//        if (null == s || !FunctionUtil.isNumber(s)) {
//            throw new FormulaParserException("文本函数AVLUE参数非法");
//        }
//        return Double.parseDouble(s);
//    }
//
//    /**
//     * 将重复数去除去
//     *
//     * @param ss 数值数组
//     * @return 数组
//     */
//    private String[] union(String[] ss) {
//        if (null == ss) {
//            throw new FormulaParserException("文本函数UNION参数非法");
//        }
//        if (ss.length <= 1) {
//            return ss;
//        }
//        List<String> r = new ArrayList<>();
//        for (String s : ss) {
//            if (!r.contains(s)) {
//                r.add(s);
//            }
//        }
//        String[] result = new String[r.size()];
//        return r.toArray(result);
//    }
//
//
//    public static void main(String[] args) {
//
//
//        TextFunction function = new TextFunction();
//        String[] ss = {"张三", "李四", "王五", "张三", "李四"};
//        System.out.println(JSON.toJSONString(function.union(ss)));
//
////        System.out.println(function.value("1000.0") + function.value("999.0"));
//
////        System.out.println(function.equals(null, null));
////        System.out.println(function.left("J234RFBGT", 10));
////        System.out.println(function.replace("JDY棒棒哒2016", 2, 4, "abc123"));
////        System.out.println(function.search("简道云", "ABC简道云2016", 0));
////        System.out.println(function.right("12AXC简道云", 5));
////        System.out.println(function.trim("      张三      李四      王五     12  1 23 1231 1 23 1231 "));
////        System.out.println(function.mid("999777199910246666", 7, 4));
////        System.out.println(function.text(new Date()));
////        System.out.println(function.dateFormat(new Date(), ""));
////        System.out.println(function.dateFormat(new Date(), "E"));
////        System.out.println(function.dateFormat(new Date(), "EE"));
////        System.out.println(function.dateFormat(new Date(), "EEE"));
////        System.out.println(function.toChar(34D) + "1");
//
////        List<Object> vs = new ArrayList<>();
////        vs.add("张三");
////        vs.add("李四");
////        vs.add("王二麻子");
////        vs.add(11);
////        vs.add(new Date());
////        System.out.println(function.splice(vs.toArray()));
//
//    }
//
//}
