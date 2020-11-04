//package org.clever.utils;
//
//import java.text.ParseException;
//import java.util.Calendar;
//import java.util.Date;
//
///**
// * Description： 时间处理工具类
// * User: 杜涛
// * Date: Created in 2020/9/25 16:48
// * Company: 杭州传化智能制造有限公司
// * Copyright: Copyright (c) 2019
// * Version: 1.0.0
// * Modified By:
// */
//public class CalendarUtil {
//
//    /**
//     * 计算两个时间相差多少个年
//     *
//     * @param start 开始时间
//     * @param end   结束时间
//     * @return 年数差
//     */
//    public static int yearsBetween(Date start, Date end) {
//        Calendar startDate = Calendar.getInstance();
//        startDate.setTime(start);
//        Calendar endDate = Calendar.getInstance();
//        endDate.setTime(end);
//        return (endDate.get(Calendar.YEAR) - startDate.get(Calendar.YEAR));
//    }
//
//    /**
//     * 计算两个时间相差多少个月
//     *
//     * @param start 开始时间
//     * @param end   结束时间
//     * @return 月数差
//     */
//    public static int monthsBetween(Date start, Date end) {
//        Calendar startDate = Calendar.getInstance();
//        startDate.setTime(start);
//        Calendar endDate = Calendar.getInstance();
//        endDate.setTime(end);
//        int result = yearsBetween(start, end) * 12 + endDate.get(Calendar.MONTH) - startDate.get(Calendar.MONTH);
//        return result == 0 ? 1 : Math.abs(result);
//    }
//
//    /**
//     * 计算两个时间相差多少个天
//     *
//     * @param start 开始时间
//     * @param end   结束时间
//     * @return 日数差
//     */
//    public static int daysBetween(Date start, Date end) {
//        return hoursBetween(start, end) / 24; // 得到两个日期相差多少天
//    }
//
//    /**
//     * 计算两个时间相差多少小时
//     *
//     * @param start 开始时间
//     * @param end   结束时间
//     * @return 小时数差
//     */
//    public static int hoursBetween(Date start, Date end) {
//        return minutesBetween(start, end) / 60; // 得到两个日期相差多少小时
//    }
//
//    /**
//     * 计算两个时间相差多少分
//     *
//     * @param start 开始时间
//     * @param end   结束时间
//     * @return 分钟数差
//     */
//    public static int minutesBetween(Date start, Date end) {
//        return secondesBetween(start, end) / 60; // 得到两个日期相差多少分
//    }
//
//    /**
//     * 计算两个时间相差多少秒
//     *
//     * @param start 开始时间
//     * @param end   结束时间
//     * @return 秒数差
//     */
//    public static int secondesBetween(Date start, Date end) {
//        Calendar startDate = Calendar.getInstance();
//        startDate.setTime(start);
//        Calendar endDate = Calendar.getInstance();
//        endDate.setTime(end);
//        startDate.set(Calendar.HOUR_OF_DAY, 0);
//        startDate.set(Calendar.MINUTE, 0);
//        startDate.set(Calendar.SECOND, 0);
//        endDate.set(Calendar.HOUR_OF_DAY, 0);
//        endDate.set(Calendar.MINUTE, 0);
//        endDate.set(Calendar.SECOND, 0);
//        // 得到两个日期相差多少秒
//        return ((int) (endDate.getTime().getTime() / 1000) - (int) (startDate.getTime().getTime() / 1000));
//    }
//
//    /**
//     * 获取指定时间的日期
//     *
//     * @param date 时间
//     * @return 第几天
//     */
//    public static int getDay(Date date) {
//        Calendar calendar = Calendar.getInstance();
//        calendar.setTime(date);
//        return calendar.get(Calendar.DAY_OF_MONTH);
//    }
//
//    /**
//     * 获取本月最大日期
//     *
//     * @param date 日期
//     * @return 最大天数
//     */
//    public static int getCurrentMonthMaxDay(Date date) {
//        Calendar calendar = Calendar.getInstance();
//        calendar.setTime(date);
//        return calendar.getActualMaximum(Calendar.DAY_OF_MONTH);
//    }
//
//    /**
//     * 获取本月最大日期
//     *
//     * @param date 日期
//     * @return true月末，false非月末
//     */
//    public static boolean isCurrentMonthLastDay(Date date) {
//        Calendar calendar = Calendar.getInstance();
//        calendar.setTime(date);
//        return calendar.getActualMaximum(Calendar.DAY_OF_MONTH) == getDay(date);
//    }
//
//    /**
//     * 计算月份
//     *
//     * @param date 等待计算的时间
//     * @param m    添加算术
//     * @return 日期对象
//     */
//    public static Date addMouth(Date date, int m) {
//        Calendar calendar = Calendar.getInstance();
//        calendar.setTime(date);
//        calendar.add(Calendar.MONTH, m);
//        return calendar.getTime();
//    }
//
//    /**
//     * 获取小时数
//     *
//     * @param date 日期
//     * @return 小时
//     */
//    public static int getHour(Date date) {
//        Calendar calendar = Calendar.getInstance();
//        calendar.setTime(date);
//        return calendar.get(Calendar.HOUR_OF_DAY);
//    }
//
//    /**
//     * 获取小时数
//     *
//     * @param date 日期
//     * @return 月数
//     */
//    public static int getMonth(Date date) {
//        Calendar calendar = Calendar.getInstance();
//        calendar.setTime(date);
//        return calendar.get(Calendar.MONTH);
//    }
//
//    /**
//     * 获取日期的在本年中的周数
//     *
//     * @param date 日期
//     * @return 周数
//     */
//    public static int getWeekNum(Date date) {
//        Calendar calendar = Calendar.getInstance();
//        calendar.setFirstDayOfWeek(Calendar.MONDAY);
//        calendar.setTime(date);
//        return calendar.get(Calendar.WEEK_OF_YEAR);
//    }
//
//    /**
//     * 获取小时数
//     *
//     * @param date 日期
//     * @return 分钟
//     */
//    public static int getMinute(Date date) {
//        Calendar calendar = Calendar.getInstance();
//        calendar.setTime(date);
//        return calendar.get(Calendar.MINUTE);
//    }
//
//    /**
//     * 获取年份
//     *
//     * @param date 日期
//     * @return 年份
//     */
//    public static int getYear(Date date) {
//        Calendar calendar = Calendar.getInstance();
//        calendar.setTime(date);
//        return calendar.get(Calendar.YEAR);
//    }
//
//    /**
//     * 获取指定日期的星期几
//     *
//     * @param date 日期
//     * @return 星期几
//     */
//    public static int getWeek(Date date) {
//        Calendar calendar = Calendar.getInstance();
//        calendar.setTime(date);
//        return calendar.get(Calendar.DAY_OF_WEEK);
//    }
//
//
//    public static void main(String[] args) throws ParseException {
////        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
////        System.out.println(CalendarUtil.monthsBetween(sdf.parse("2010-12-12 12:32:44"), sdf.parse("2020-11-12 12:32:44")));
//        System.out.println(CalendarUtil.getWeek(new Date()));
//
//    }
//
//}
