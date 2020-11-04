//package org.clever.function;
//
//import com.etransfar.tims3.common.exception.TimsException;
//import com.etransfar.tims3.common.utils.DateUtil;
//import com.etransfar.tims3.s2.auth.formula.parser.FormulaParserException;
//import com.etransfar.tims3.s2.auth.formula.utils.CalendarUtil;
//
//import java.util.Date;
//
///**
// * Description： 日期函数
// * User: 沐彦
// * Date: Created in 2020/9/25 15:05
// * Version: 1.0.0
// * Modified By:
// */
//public class DateFunction {
//
//    /**
//     * 将时间戳转换为日期对象
//     *
//     * @param v 将时间戳和日期字符串格式
//     * @return 时间对象
//     */
//    private Date date(Object v) {
//        if (v instanceof String) {
//            Date d = null;
//            String[] formats = new String[]{"yyyy-MM-dd HH:mm:ss", "yyyy-MM-dd HH:mm", "yyyy-MM-dd HH", "yyyy-MM-dd", "yyyy-MM-dd HH:mm:ss.SSS"};
//            for (String format : formats) {
//                d = DateUtil.parseDateStr(v.toString(), format);
//                if (null != d) break;
//            }
//            if (null == d) {
//                throw new FormulaParserException("日期函数date计算失败");
//            }
//            return d;
//        } else if (v instanceof Long || v instanceof Integer || v instanceof Double || v instanceof Float) {
//            return new Date(new Double(v.toString()).longValue());
//        } else if (v instanceof Date) {
//            return (Date) v;
//        }
//        throw new TimsException("日期函数date计算参数非法");
//    }
//
//    /**
//     * 将指定日期加/减指定天数
//     *
//     * @param v 将时间戳和日期字符串格式
//     * @param i 加减的天数
//     * @return
//     */
//    private Date datedelta(Object v, Long i) {
//        if (null == v || null == i) {
//            throw new FormulaParserException("日期函数DATEDELTA计算参数非法");
//        }
//        return DateUtil.timeAddDay(date(v), i);
//    }
//
//    /**
//     * 计算两个日期时间相差的年数、月数、天数、小时数、分钟数、秒数
//     *
//     * @param v1 开始时间
//     * @param v2 结束时间
//     * @param f  日期格式
//     * @return 计算天数
//     */
//    private int datedif(Object v1, Object v2, String f) {
//        if (null == v1 || null == v2) {
//            throw new FormulaParserException("日期函数DATEDIF日期格式参数非法");
//        }
//        Date startDate = date(v1);
//        Date endDate = date(v2);
//        switch (f) {
//            case "y":
//                return CalendarUtil.yearsBetween(startDate, endDate);
//            case "M":
//                return CalendarUtil.monthsBetween(startDate, endDate);
//            case "d":
//                return CalendarUtil.daysBetween(startDate, endDate);
//            case "h":
//                return CalendarUtil.hoursBetween(startDate, endDate);
//            case "m":
//                return CalendarUtil.minutesBetween(startDate, endDate);
//            case "s":
//                return CalendarUtil.secondesBetween(startDate, endDate);
//            default:
//                throw new TimsException("日期函数DATEDIF日期格式参数非法");
//        }
//    }
//
//    /**
//     * 获取日期
//     *
//     * @param v1 日期
//     * @return 天数
//     */
//    private int day(Object v1) {
//        if (null == v1) {
//            throw new TimsException("日期函数DAY入参非法");
//        }
//        return CalendarUtil.getDay(date(v1));
//    }
//
//    /**
//     * 计算两个日期的天数差
//     *
//     * @param v1 日期1
//     * @param v2 日期2
//     * @return 天数差
//     */
//    private int days(Object v1, Object v2) {
//        if (null == v1 || null == v2) {
//            throw new TimsException("日期函数DAYS入参非法");
//        }
//        return CalendarUtil.daysBetween(date(v1), date(v2));
//    }
//
//    /**
//     * 按照一年 360 天的算法，返回两个日期间相差的天数
//     *
//     * @param v1 结束日期
//     * @param v2 起始日期
//     * @param f  起始日期
//     * @return 相差天数
//     */
//    private int days360(Object v1, Object v2, Boolean f) {
//        if (null == v1 || null == v2) {
//            throw new TimsException("日期函数DAYS360入参非法");
//        }
//        Date startDate = date(v1);
//        Date startLastDate = CalendarUtil.addMouth(startDate, -1);
//        Date endDate = date(v2);
//        Date endLastDate = CalendarUtil.addMouth(endDate, -1);
//        int mouthCount = CalendarUtil.monthsBetween(startLastDate, endLastDate);
//        int startDay;
//        int endDay;
//        if (null == f || !f) { // 美国标准
//            if (CalendarUtil.isCurrentMonthLastDay(startDate)
//                    && (31 == CalendarUtil.getCurrentMonthMaxDay(startDate)
//                    || 29 == CalendarUtil.getCurrentMonthMaxDay(startDate)
//                    || 28 == CalendarUtil.getCurrentMonthMaxDay(startDate))) {
//                startDay = 30;
//            } else {
//                startDay = CalendarUtil.getDay(startDate);
//            }
//            if (CalendarUtil.isCurrentMonthLastDay(endDate) && startDay < 30) {
//                endDay = 31;
//            } else {
//                endDay = CalendarUtil.getDay(endDate);
//            }
//        } else { // 欧洲标准
//            if (CalendarUtil.isCurrentMonthLastDay(startDate) || 31 == CalendarUtil.getCurrentMonthMaxDay(startDate)) {
//                startDay = 30;
//            } else {
//                startDay = CalendarUtil.getDay(startDate);
//            }
//            if (CalendarUtil.isCurrentMonthLastDay(endDate) || 31 == CalendarUtil.getCurrentMonthMaxDay(endDate)) {
//                endDay = 30;
//            } else {
//                endDay = CalendarUtil.getDay(endDate);
//            }
//        }
//        return mouthCount * 30 + endDay - startDay;
//    }
//
//    /**
//     * 获取当前日期的小时数
//     *
//     * @param v 日期
//     * @return 小时
//     */
//    private int hour(Object v) {
//        if (null == v) {
//            throw new TimsException("日期函数HOUR计算参数非法");
//        }
//        return CalendarUtil.getHour(date(v));
//    }
//
//    /**
//     * 获取当前日期的月份数
//     *
//     * @param v 日期
//     * @return 小时
//     */
//    private int month(Object v) {
//        if (null == v) {
//            throw new TimsException("日期函数MOUTH计算参数非法");
//        }
//        return CalendarUtil.getMonth(date(v));
//    }
//
//    /**
//     * 返回某日期的分钟数
//     *
//     * @param v 日期
//     * @return 分钟数
//     */
//    private int minute(Object v) {
//        if (null == v) {
//            throw new TimsException("日期函数MINUTE计算参数非法");
//        }
//        return CalendarUtil.getMinute(date(v));
//    }
//
//    /**
//     * 当天日期
//     *
//     * @return 当天日期
//     */
//    private Date today() {
//        return new Date();
//    }
//
//    /**
//     * 获取当前服务器时间
//     *
//     * @return 时间对象
//     */
//    private Date systime() {
//        // TODO 临时使用当前系统时间
//        return new Date();
//    }
//
//
//    /**
//     * 返回特定时间的十进制数字
//     *
//     * @param hour   小时
//     * @param minute 分钟
//     * @param second 秒钟
//     * @return 返回值
//     */
//    private double time(int hour, int minute, int second) {
//        return (new Double(hour * 60 * 60 + minute * 60 + second).doubleValue()) / (24 * 60 * 60);
//    }
//
//    /**
//     * 将日期对象转换成时间戳
//     *
//     * @param v 传入值
//     * @return 时间戳
//     */
//    private long timestamp(Object v) {
//        if (null == v) {
//            throw new TimsException("日期函数timestamp计算参数费非法");
//        }
//        return date(v).getTime();
//    }
//
//    /**
//     * 返回指定日期在当年是第几周
//     *
//     * @param v 日期
//     * @return 周数
//     */
//    private int weeknum(Object v) {
//        if (null == v) {
//            throw new TimsException("日期函数weeknum计算参数费非法");
//        }
//        return CalendarUtil.getWeekNum(date(v));
//    }
//
//    /**
//     * 获取年份
//     *
//     * @param v 日期
//     * @return 年数
//     */
//    private int year(Object v) {
//        if (null == v) {
//            throw new TimsException("日期函数year计算参数费非法");
//        }
//        return CalendarUtil.getYear(date(v));
//    }
//
//}
