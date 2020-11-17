package org.clever;

import com.alibaba.fastjson.JSON;
import org.clever.calc.Formula;
import org.clever.calc.Variable;

import java.util.StringTokenizer;

public class Test {

    public static void main(String[] args) {
//        String formula = "SQRT(SUM(x, y, xy, 54, 23, 6))";
//        Formula f = new Formula(formula, new Variable("x", "COUNT(1, 3, 2)"), new Variable("y", 3), new Variable("xy", 4));

        Formula formula = new Formula("AND(1, 2, 3)");
        System.out.println(formula.calculation());
    }

}
