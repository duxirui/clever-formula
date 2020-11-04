package org.clever.enums;

/**
 * Description：公式元素类型
 * User: 沐彦
 * Date: Created in 2020/10/6 09:15
 * Version: 1.0.0
 * Modified By:
 */
public enum ElementType {

    STRING,                 // 字符串
    APOSTROPHE,             // 单引号字符串
    NUMBER,                 // 数字
    BOOLEAN,                // 布尔
    FUNCTION,               // 函数
    ARRAY,                  // 数组
    BRACKETS,               // 括号
    VARIABLE,               // 变量
    EXPRESSION,             // 表达式

    ADDITION,               // 加法
    SUBTRACTION,            // 减法
    MULTIPLICATION,         // 乘法
    DIVISION,               // 除法
    MORE_THAN,              // 大于
    MORE_THAN_OR_EQUAL,     // 大于等于
    LESS_THAN,              // 小于
    LESS_THAN_OR_EQUAL,     // 小于等于
    EQUAL,                  // 等于
    NOT_EQUAL;              // 不等于

}
