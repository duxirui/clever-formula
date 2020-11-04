package org.clever.function;

import org.clever.calc.Formula;
import org.clever.exception.FormulaParserException;
import org.clever.exception.FunctionFormulaCalcException;
import org.clever.utils.FormulaUtil;

import java.math.RoundingMode;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * Description： 数学计算公式
 * User: 沐彦
 * Date: Created in 2020/9/24 08:52
 * Version: 1.0.0
 * Modified By:
 */
public class MathFunction {

    /**
     * 绝对值计算
     *
     * @param v 计算数值（输入）
     * @return 计算后的值（输出）
     */
    private double abs(double v) {
        return Math.abs(v);
    }

    /**
     * 平均值计算
     *
     * @param vs 输入参数（输入）
     * @return 计算后的值（输出）
     */
    private double average(Double[] vs) {
        int size;
        if (null == vs || 0 == (size = vs.length)) {
            return 0;
        }
        double total = 0;
        for (Double v : vs) {
            if (null == v) continue;
            total += v;
        }
        return total / size;
    }

    /**
     * 可以将数字增大到最接近原值的指定因数的倍数
     * ceiling(7,6)返回12，因为12比7大的同时，也是6的倍数中最接近7的数字
     *
     * @param dx 基数
     * @param dy 因数
     * @return 计算结果
     */
    private double ceiling(double dx, double dy) {
        int x = FormulaUtil.myRound(dx);
        int y = FormulaUtil.myRound(dy);
        for (int i = 0; i < Integer.MAX_VALUE; i++) {
            int r;
            if ((r = i * y) >= x) {
                return r;
            }
        }
        return -1;
    }

    /**
     * 将数字减小到最接近原值的指定因数的倍数
     *
     * @param dx 数字
     * @param dy 因数
     * @return 输出结果
     */
    private double floor(double dx, double dy) {
        int x = FormulaUtil.myRound(dx);
        int y = FormulaUtil.myRound(dy);
        if (x <= y) {
            throw new FormulaParserException("数学函数floor计算参数非法");
        }
        return x - (x % y);
    }

    /**
     * 统计
     *
     * @param vs 输入数组
     * @return 输出结果
     */
    private double count(Object[] vs) {
        if (null == vs || 0 == vs.length) {
            return 0;
        }
        return vs.length;
    }

    /**
     * 按照条件判断统计
     *
     * @param vs         数据
     * @param expression 表达式
     * @return 统计数量
     */
    private double countif(Double[] vs, String expression) {
        if (null == vs) {
            return -1;
        }
        List<Double> vsList = new ArrayList<>();
        for (Double v : vs) {
            Formula e = new Formula(v + expression);
            Object result = e.calculation();
            if (!(result instanceof Boolean)) {
                continue;
            }
            if ((Boolean) result) {
                vsList.add(v);
            }
        }
        return vsList.size();
    }

    /**
     * 将数字舍入到指定的小数位数并输出
     *
     * @param v       数字
     * @param dDigits 小数位数
     * @return 格式化后的结果
     */
    private double fixed(Double v, double dDigits) {
        int digits = FormulaUtil.myRound(dDigits);
        if (null == v || digits < 0) {
            throw new FunctionFormulaCalcException("Mathematical formula fixed calculation parameter is illegal");
        }
        StringBuilder format = new StringBuilder("#.");
        for (int i = 0; i < digits; i++) {
            format.append("0");
        }
        DecimalFormat decimalFormat = new DecimalFormat(format.toString());
        decimalFormat.setRoundingMode(RoundingMode.FLOOR);
        return Double.parseDouble(decimalFormat.format(v));
    }

    /**
     * 将数字舍入到指定的小数位数并输出
     *
     * @param v       数字
     * @param dDigits 小数位数
     * @return 格式化后的结果
     */
    private double round(Double v, double dDigits) {
        int digits = FormulaUtil.myRound(dDigits);
        if (null == v || digits < 0) {
            throw new FunctionFormulaCalcException("Mathematical formula round calculation parameter is illegal");
        }
        StringBuilder format = new StringBuilder("#.");
        for (int i = 0; i < digits; i++) {
            format.append("0");
        }
        DecimalFormat decimalFormat = new DecimalFormat(format.toString());
        return Double.parseDouble(decimalFormat.format(v));
    }

    /**
     * 获取整数
     *
     * @param v 数字
     * @return 计算后数字
     */
    public long toTnt(Double v) {
        if (null == v) {
            throw new FunctionFormulaCalcException("The mathematical formula int calculation parameter is illegal");
        }
        return v.longValue();
    }

    /**
     * 获取数据集中第k个最大值
     *
     * @param vs 数字数字
     * @param dk 取数个数
     * @return 返回值
     */
    private double large(Double[] vs, double dk) {
        int k = FormulaUtil.myRound(dk);
        if (null == vs || 0 == vs.length || k < 1 || vs.length < k) {
            throw new FunctionFormulaCalcException("Mathematical formula large calculation parameter is invalid");
        }
        Arrays.sort(vs);
        return vs[vs.length - k];
    }

    /**
     * 根据指定底数返回数字的对数
     *
     * @param v 数字
     * @param d 底数
     * @return 对数
     */
    private double log(Double v, Double d) {
        if (null == v || null == d) {
            throw new FunctionFormulaCalcException("Mathematical formula log calculation parameter is illegal");
        }
        return Math.log(v) / Math.log(d);
    }

    /**
     * 获取一组数值的最大值
     *
     * @param vs 数组数组
     * @return 最大值
     */
    private double max(Double[] vs) {
        if (null == vs || vs.length <= 0) {
            throw new FunctionFormulaCalcException("Mathematical formula max calculation parameter is illegal");
        }
        Arrays.sort(vs);
        return vs[vs.length - 1];
    }

    /**
     * 获取一组数值的最小值
     *
     * @param vs 数组数组
     * @return 最小值
     */
    private double min(Double[] vs) {
        if (null == vs || vs.length <= 0) {
            throw new FunctionFormulaCalcException("Mathematical formula min calculation parameter is illegal");
        }
        Arrays.sort(vs);
        return vs[0];
    }

    /**
     * 获取两数相除的余数
     *
     * @param v 数字
     * @param m 指数
     * @return 余数
     */
    private double mod(Double v, Double m) {
        if (null == v || null == m) {
            throw new FunctionFormulaCalcException("Mathematical formula mod calculation parameter is illegal");
        }
        return v % m;
    }

    /**
     * 计算次幂
     *
     * @param v 数字
     * @param m 幂
     * @return 计算结果
     */
    private double power(Double v, Double m) {
        if (null == v || null == m) {
            throw new FunctionFormulaCalcException("Mathematical formula power calculation parameter is illegal");
        }
        return Math.pow(v, m);
    }

    /**
     * 获取一组数值的乘积
     *
     * @param vs 数值数组
     * @return 计算结果
     */
    private double product(Double[] vs) {
        if (null == vs || vs.length <= 0) {
            throw new FunctionFormulaCalcException("Mathematical formula product calculation parameter is illegal");
        }
        Double r = null;
        for (Double v : vs) {
            if (null == r)
                r = v;
            else
                r *= v;
        }
        return r;
    }

    /**
     * 获取0到1的随机数
     *
     * @return 随机数
     */
    private double rand() {
        return Math.random();
    }

    /**
     * 获取指定数值集合中第k个小的数字
     *
     * @param vs 数值集合
     * @param dk 个数
     * @return 返回数据
     */
    private double small(Double[] vs, double dk) {
        int k = FormulaUtil.myRound(dk);
        if (null == vs || 0 == vs.length || k < 1 || vs.length < k) {
            throw new FunctionFormulaCalcException("Mathematical formula large calculation parameter is illegal");
        }
        Arrays.sort(vs);
        return vs[k - 1];
    }

    /**
     * 获取一个数字的正平方根
     *
     * @param v 数值
     * @return 计算后结果
     */
    private double sqrt(Double v) {
        if (null == v) {
            throw new FunctionFormulaCalcException("Mathematical formula sqrt calculation parameter is illegal");
        }
        return Math.sqrt(v);
    }

    /**
     * 获取一组数值的总和
     *
     * @param vs 数值数组
     * @return 计算结果
     */
    private double sum(Double[] vs) {
        if (null == vs || vs.length <= 0) {
            throw new FunctionFormulaCalcException("Mathematical formula sum calculation parameter is illegal");
        }
        double r = 0D;
        for (Double v : vs) {
            r += v;
        }
        return r;
    }

    /**
     * 将数组间对应的元素相乘，并返回乘积之和，适用于加权求和
     *
     * @param vs1 第一个数值数组
     * @param vs2 第二个数值数组
     * @return 和
     */
    private double sumproduct(Double[] vs1, Double[] vs2) {
        if (null == vs1 || null == vs2 || vs1.length <= 0 || vs2.length <= 0) {
            throw new FunctionFormulaCalcException("Mathematical formula sumproduct calculation parameter is illegal");
        }
        int maxLength = vs1.length;
        if (maxLength > vs2.length) {
            maxLength = vs2.length;
        }
        double r = 0;
        for (int i = 0; i < maxLength; i++) {
            r += vs1[i] * vs2[i];
        }
        return r;
    }

}



