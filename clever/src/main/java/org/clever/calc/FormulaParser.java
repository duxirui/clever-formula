package org.clever.calc;


import com.alibaba.fastjson.JSON;
import org.clever.enums.ElementType;
import org.clever.exception.FormulaParserException;
import org.clever.function.Function;
import org.clever.utils.FormulaUtil;

import java.util.*;

import static org.clever.utils.FormulaUtil.isNumber;

/**
 * Description： 公式解析器
 * User: 杜涛
 * Date: Created in 2020/10/6 08:55
 * Version: 1.0.0
 * Modified By:
 */
class FormulaParser {

    protected Map<String, String> variableMaps = new LinkedHashMap<>();

    /**
     * 进行函数计算
     *
     * @param formula 公式
     * @return 计算结果
     */
    protected Object calc(String formula) {
        if (null == formula || "".equals(formula.trim())) {
            throw new FormulaParserException("Calculation formula cannot be empty");
        }
        List<FormulaElement> elements = formulaCut(formula);
        if (null == elements || elements.size() == 0) {
            throw new FormulaParserException("No operator available for calculation formula");
        }
        return handleCalcElement(elements);
    }

    /**
     * 计算公式分割
     *
     * @param formula 计算公式
     * @return 分解对象集合
     * @throws FormulaParserException 公式解析异常
     */
    private List<FormulaElement> formulaCut(String formula) throws FormulaParserException {
        try {
            List<FormulaElement> elements = new ArrayList<>();
            for (int index = 0; index < formula.length(); index++) {
                Character c = formula.charAt(index);
                StringBuilder element = new StringBuilder().append(c);
                if ((c.equals(' ') || c.equals(','))) {
                    continue;
                }
                ElementType currentType = handleCurrentElementType(c);
                if (ElementType.BOOLEAN.equals(currentType)) {
                    int max = 4;
                    if (c.equals('t')) {
                        max = 3;
                    }
                    for (int i = 1; i <= max; i++) {
                        element.append(formula.charAt(index + i));
                    }
                    if (Boolean.TRUE.toString().equalsIgnoreCase(element.toString()) || Boolean.FALSE.toString().equalsIgnoreCase(element.toString())) {
                        index += max;
                    } else {
                        throw new FormulaParserException("Illegal characters in formula:" + c);
                    }
                } else if (ElementType.MORE_THAN.equals(currentType) || ElementType.LESS_THAN.equals(currentType)) {
                    Character tempChar = formula.charAt(index + 1);
                    if (tempChar.equals('=')) {
                        if (ElementType.MORE_THAN.equals(currentType)) {
                            currentType = ElementType.MORE_THAN_OR_EQUAL;
                        } else {
                            currentType = ElementType.LESS_THAN_OR_EQUAL;
                        }
                        element.append(tempChar);
                        index++;
                    }
                }

                int start = 0, end = 0;
                while (true) {
                    if (!isWhile(currentType)) break;
                    index++;
                    if (index >= formula.length()) break;
                    c = formula.charAt(index);
                    if (ElementType.FUNCTION.equals(currentType) || ElementType.BRACKETS.equals(currentType)) {
                        element.append(c);
                        if (ElementType.BRACKETS.equals(currentType) && 0 == start) {
                            start = 1;
                        }
                        if (c.equals('(')) {
                            start++;
                        } else if (c.equals(')')) {
                            end++;
                        }
                        if (start > 0 && start == end) {
                            break;
                        }
                    } else if (ElementType.STRING.equals(currentType)) {
                        element.append(c);
                        if (c.equals('"')) {
                            break;
                        }
                    } else if (ElementType.APOSTROPHE.equals(currentType)) {
                        element.append(c);
                        if (c.equals('\'')) {
                            break;
                        }
                    } else if (ElementType.NUMBER.equals(currentType)) {
                        if (!isNumber(element.toString() + c)) {
                            index--;
                            break;
                        }
                        element.append(c);
                        int nextIndex = index + 1;
                        if (formula.length() <= nextIndex) {
                            break;
                        }
                        Character tempChar = formula.charAt(nextIndex);
                        if (tempChar.equals('+') || tempChar.equals('-') || tempChar.equals('*') || tempChar.equals('/') || tempChar.equals(',')) {
                            break;
                        }
                    } else if (ElementType.ARRAY.equals(currentType)) {
                        element.append(c);
                        if (0 == start) {
                            start = 1;
                        }
                        if (c.equals('[')) {
                            start++;
                        } else if (c.equals(']')) {
                            end++;
                        }
                        if (start > 0 && start == end) {
                            break;
                        }
                    } else if (ElementType.EQUAL.equals(currentType) || ElementType.NOT_EQUAL.equals(currentType)) {
                        if (!c.equals('=')) {
                            index--;
                            break;
                        }
                        element.append(c);
                    } else if (ElementType.VARIABLE.equals(currentType)) {
                        if (c.equals('+') || c.equals('-') || c.equals('*') || c.equals('/') || c.equals('>') || c.equals('<') || c.equals(',')) {
                            index--;
                            break;
                        }
                        element.append(c);
                    }
                }
                String elementVal = element.toString().trim();
                FormulaElement currentElement = handleElement(currentType, elementVal);
                if (ElementType.EQUAL.equals(currentElement.getType())) {
                    if (String.valueOf(currentElement.getContent()).length() > 2) {
                        throw new FormulaParserException("The expression is wrong, please use the correct expression:'='or'=='");
                    }
                }
                if (ElementType.NOT_EQUAL.equals(currentElement.getType())) {
                    if (String.valueOf(currentElement.getContent()).length() != 2) {
                        throw new FormulaParserException("The expression is wrong, please use the correct expression:'!='");
                    }
                }
                if (ElementType.VARIABLE.equals(currentElement.getType())) {
                    String varFormula = variableMaps.get(String.valueOf(currentElement.getContent()));
                    if (null == varFormula || varFormula.trim().equals("")) {
                        throw new FormulaParserException("Value definition not found for variable " + currentElement.getContent());
                    }
                    currentElement.setType(ElementType.BRACKETS);
                    currentElement.setContent(formulaCut(varFormula));
                }
                if (null != elements && elements.size() > 2
                        && (ElementType.MULTIPLICATION.equals(elements.get(elements.size() - 1).getType())
                        || ElementType.DIVISION.equals(elements.get(elements.size() - 1).getType()))) { // 处理加减乘除的算术优先级
                    FormulaElement tmpElement = elements.get(elements.size() - 3);
                    if (ElementType.ADDITION.equals(tmpElement.getType()) || ElementType.SUBTRACTION.equals(tmpElement.getType())) {
                        elements.add(FormulaElement.getInstance(ElementType.BRACKETS, handelElementAndBody(elements, currentElement)));
                    } else if (ElementType.MULTIPLICATION.equals(tmpElement.getType()) || ElementType.DIVISION.equals(tmpElement.getType())) {
                        elements.add(currentElement);
                    }
                } else if (null != elements && elements.size() > 1
                        && (ElementType.EQUAL.equals(elements.get(elements.size() - 1).getType())
                        || ElementType.NOT_EQUAL.equals(elements.get(elements.size() - 1).getType())
                        || ElementType.MORE_THAN.equals(elements.get(elements.size() - 1).getType())
                        || ElementType.MORE_THAN_OR_EQUAL.equals(elements.get(elements.size() - 1).getType())
                        || ElementType.LESS_THAN.equals(elements.get(elements.size() - 1).getType())
                        || ElementType.LESS_THAN_OR_EQUAL.equals(elements.get(elements.size() - 1).getType()))
                ) { // 处理表达式
                    elements.add(FormulaElement.getInstance(ElementType.EXPRESSION, handelElementAndBody(elements, currentElement)));
                } else {
                    elements.add(currentElement);
                }
            }
            return elements;
        } catch (Exception e) {
            e.printStackTrace();
            throw new FormulaParserException("The expression is wrong, please use the correct expression");
        }
    }

    /**
     * 是否进行循环处理
     *
     * @param currentType 元素类型
     * @return 是否可以循环
     */
    private boolean isWhile(ElementType currentType) {
        if (ElementType.STRING.equals(currentType) || ElementType.APOSTROPHE.equals(currentType)
                || ElementType.FUNCTION.equals(currentType) || ElementType.NUMBER.equals(currentType)
                || ElementType.BRACKETS.equals(currentType) || ElementType.ARRAY.equals(currentType)
                || ElementType.EQUAL.equals(currentType) || ElementType.NOT_EQUAL.equals(currentType)
                || ElementType.VARIABLE.equals(currentType)) {
            return true;
        }
        return false;
    }

    /**
     * 处理当前元素类型
     *
     * @param c 字符
     * @return 当前元素类型
     */
    private ElementType handleCurrentElementType(Character c) {
        ElementType currentType;
        if (c.equals('(')) {
            currentType = ElementType.BRACKETS;
        } else if (c.equals('"')) {
            currentType = ElementType.STRING;
        } else if (c.equals('\'')) {
            currentType = ElementType.APOSTROPHE;
        } else if (c.equals('[')) {
            currentType = ElementType.ARRAY;
        } else if ((c >= 'A' && c <= 'Z')) {
            currentType = ElementType.FUNCTION;
        } else if (((c >= '0' && c <= '9'))) {
            currentType = ElementType.NUMBER;
        } else if (c.equals('+')) {
            currentType = ElementType.ADDITION;
        } else if (c.equals('-')) {
            currentType = ElementType.SUBTRACTION;
        } else if (c.equals('*')) {
            currentType = ElementType.MULTIPLICATION;
        } else if (c.equals('/')) {
            currentType = ElementType.DIVISION;
        } else if (c.equals('=')) {
            currentType = ElementType.EQUAL;
        } else if (c.equals('!')) {
            currentType = ElementType.NOT_EQUAL;
        } else if (c.equals('t') || c.equals('f')) {
            currentType = ElementType.BOOLEAN;
        } else if (c.equals('>')) {
            currentType = ElementType.MORE_THAN;
        } else if (c.equals('<')) {
            currentType = ElementType.LESS_THAN;
        } else {
            currentType = ElementType.VARIABLE;
        }
        return currentType;
    }

    /**
     * 处理元素并返回Body
     *
     * @param elements       元素列表
     * @param currentElement 当前元素
     * @return 返回二级计算节点集合
     */
    private List<FormulaElement> handelElementAndBody(List<FormulaElement> elements, FormulaElement currentElement) {
        List<FormulaElement> body = new ArrayList<>();
        body.add(elements.get(elements.size() - 2));
        body.add(elements.get(elements.size() - 1));
        body.add(currentElement);
        elements.remove(elements.get(elements.size() - 1));
        elements.remove(elements.get(elements.size() - 1));
        return body;
    }

    /**
     * 处理计算元素
     *
     * @param currentType 当前类型
     * @param elementVal  元素值
     * @return 计算结果
     * @throws FormulaParserException 公式解析异常
     */
    private FormulaElement handleElement(ElementType currentType, String elementVal) throws FormulaParserException {
        if (ElementType.BRACKETS.equals(currentType)) {
            List<FormulaElement> subElements = formulaCut(elementVal.substring(1, elementVal.length() - 1));
            return FormulaElement.getInstance(currentType, elementVal, subElements);
        } else if (ElementType.STRING.equals(currentType) || ElementType.APOSTROPHE.equals(currentType)) {
            return FormulaElement.getInstance(currentType, elementVal.substring(1, elementVal.length() - 1));
        } else if (ElementType.FUNCTION.equals(currentType)) {
            List<FormulaElement> paramElements = formulaCut(elementVal.substring(elementVal.indexOf("(") + 1, elementVal.length() - 1));
            return FormulaElement.getInstance(currentType, elementVal.substring(0, elementVal.indexOf("(")), paramElements);
        } else if (ElementType.ARRAY.equals(currentType)) {
            List<Object> list = JSON.parseArray(elementVal, Object.class);
            for (int i = 0; i < list.size(); i++) {
                Object o = list.get(i);
                if (isNumber(String.valueOf(o))) {
                    list.set(i, new Double(String.valueOf(o)));
                }
            }
            return FormulaElement.getInstance(currentType, list.toArray());
        } else {
            return FormulaElement.getInstance(currentType, elementVal);
        }
    }

    /**
     * 处理计算元素
     *
     * @param elements 元素集合
     * @return 计算结果
     */
    private Object handleCalcElement(List<FormulaElement> elements) {
        if (null == elements || elements.size() == 0) {
            throw new FormulaParserException("Formula calculation element cannot be null!");
        }
        Object result = 0D;
        ElementType currentOperator = null;
        for (FormulaElement e : elements) {
            // 运算符
            if (ElementType.ADDITION.equals(e.getType()) || ElementType.SUBTRACTION.equals(e.getType())
                    || ElementType.MULTIPLICATION.equals(e.getType()) || ElementType.DIVISION.equals(e.getType())) {
                currentOperator = e.getType();
            } else if (ElementType.NUMBER.equals(e.getType())) { // 数字
                double second = Double.parseDouble(String.valueOf(e.getContent()));
                if (null == currentOperator) {
                    result = Double.valueOf(String.valueOf(e.getContent()));
                } else if (result instanceof Double) {
                    double first = ((Double) result);
                    result = calcDoubleType(currentOperator, first, second);
                } else if (result instanceof String) {
                    String first = String.valueOf(result);
                    if (ElementType.ADDITION.equals(currentOperator)) {
                        result = first + second;
                    } else {
                        throw new FormulaParserException("String type cannot participate in arithmetic operations");
                    }
                }
            } else if (ElementType.STRING.equals(e.getType()) || ElementType.APOSTROPHE.equals(e.getType())) {
                if (null == currentOperator) {
                    result = String.valueOf(e.getContent());
                } else if (ElementType.ADDITION.equals(currentOperator)) {
                    result = String.valueOf(result) + e.getContent();
                } else {
                    throw new FormulaParserException("String type cannot participate in arithmetic operations : " + currentOperator);
                }
            } else if (ElementType.BRACKETS.equals(e.getType())) { // 括号
                List<FormulaElement> subElements = (List<FormulaElement>) e.getContent();
                Object o = handleCalcElement(subElements);
                result = calcChild(currentOperator, result, o);
            } else if (ElementType.EXPRESSION.equals(e.getType())) {
                List<FormulaElement> subElements = (List<FormulaElement>) e.getContent();
                if (null != subElements && subElements.size() > 0) {
                    Object o1 = handleCalcElement(Arrays.asList(subElements.get(0)));
                    Object o2 = handleCalcElement(Arrays.asList(subElements.get(2)));
                    FormulaElement expressionElement = subElements.get(1);
                    Boolean b = null;
                    if (ElementType.EQUAL.equals(expressionElement.getType())) {
                        if (o1 instanceof Boolean && o2 instanceof Double) {
                            o2 = FormulaUtil.toBool(o2);
                        } else if (o1 instanceof Double && o2 instanceof Boolean) {
                            o1 = FormulaUtil.toBool(o1);
                        }
                        b = o1.equals(o2);
                    } else if (ElementType.NOT_EQUAL.equals(expressionElement.getType())) {
                        if (o1 instanceof Boolean && o2 instanceof Double) {
                            o2 = FormulaUtil.toBool(o2);
                        } else if (o1 instanceof Double && o2 instanceof Boolean) {
                            o1 = FormulaUtil.toBool(o1);
                        }
                        b = !o1.equals(o2);
                    } else if (ElementType.MORE_THAN.equals(expressionElement.getType())) {
                        if (!(o1 instanceof Double) || !(o2 instanceof Double)) {
                            throw new FormulaParserException("Non-numeric elements cannot be compared");
                        }
                        return ((Double) o1) > ((Double) o2);
                    } else if (ElementType.MORE_THAN_OR_EQUAL.equals(expressionElement.getType())) {
                        if (!(o1 instanceof Double) || !(o2 instanceof Double)) {
                            throw new FormulaParserException("Non-numeric elements cannot be compared");
                        }
                        return ((Double) o1) >= ((Double) o2);
                    } else if (ElementType.LESS_THAN.equals(expressionElement.getType())) {
                        if (!(o1 instanceof Double) || !(o2 instanceof Double)) {
                            throw new FormulaParserException("Non-numeric elements cannot be compared");
                        }
                        return ((Double) o1) < ((Double) o2);
                    } else if (ElementType.LESS_THAN_OR_EQUAL.equals(expressionElement.getType())) {
                        if (!(o1 instanceof Double) || !(o2 instanceof Double)) {
                            throw new FormulaParserException("Non-numeric elements cannot be compared");
                        }
                        return ((Double) o1) <= ((Double) o2);
                    }
                    if (null == b) {
                        throw new FormulaParserException("Illegal expression");
                    }
                    if (null == currentOperator) {
                        result = b;
                    } else if (result instanceof String) {
                        result = String.valueOf(result) + b;
                    } else {
                        throw new FormulaParserException("Boolean types cannot participate in arithmetic operations");
                    }
                }
            } else if (ElementType.FUNCTION.equals(e.getType())) { // 函数
                Function function = Function.get(String.valueOf(e.getContent()));
                List<FormulaElement> paramElements = e.getParams();
                List<Object> params = new ArrayList<>();
                if (1 == function.getParamNum() && 0 == function.getFixedParamNum()) { // 只有一个可变参数
                    List<Object> tempList = new ArrayList<>();
                    for (int index = 0; index < paramElements.size(); index++) {
                        index += handleFunctionParams(paramElements, index, tempList);
                    }
                    if (null != tempList && tempList.size() > 0) {
                        if (tempList.get(0).getClass().getTypeName().endsWith("[]")) {
                            params.addAll(tempList);
                        } else {
                            if (null != tempList && tempList.size() > 0 && tempList.get(0) instanceof Double) {
                                Double[] ds = new Double[tempList.size()];
                                for (int i = 0; i < tempList.size(); i++) {
                                    ds[i] = new Double(String.valueOf(tempList.get(i)));
                                }
                                params.add(ds);
                            } else {
                                params.add(tempList.toArray());
                            }
                        }
                    }
                } else {
                    for (int index = 0; index < paramElements.size(); index++) {
                        index += handleFunctionParams(paramElements, index, params);
                    }
                }
                Object o = function.calc(params.toArray());
                result = calcChild(currentOperator, result, o);
            }
        }
        return result;
    }

    /**
     * 计算double类型数据
     *
     * @param type   运算类型
     * @param first  第一个算子
     * @param second 第二个算子
     * @return 结算结果
     */
    private Object calcDoubleType(ElementType type, double first, double second) {
        if (null == type) {
            return second;
        } else if (ElementType.ADDITION.equals(type)) {
            return first + second;
        } else if (ElementType.SUBTRACTION.equals(type)) {
            return first - second;
        } else if (ElementType.MULTIPLICATION.equals(type)) {
            return first * second;
        } else if (ElementType.DIVISION.equals(type)) {
            return first / second;
        } else {
            return second;
        }
    }

    /**
     * 判断和计算子内容，针对括号和函数体
     *
     * @param type   运算类型
     * @param result 结果
     * @param o      当前步骤的计算结果
     * @return 计算结果
     */
    private Object calcChild(ElementType type, Object result, Object o) {
        if (null == type) {
            return o;
        } else if (o instanceof Double && result instanceof Double) {
            double first = ((Double) result);
            double second = Double.parseDouble(String.valueOf(o));
            return calcDoubleType(type, first, second);
        } else if (result instanceof String && !ElementType.ADDITION.equals(type)) {
            throw new FormulaParserException("String type cannot participate in arithmetic operations");
        } else { // 字符串&字符串    字符串&数字   数字&字符串
            return String.valueOf(result) + o;
        }
    }

    /**
     * 处理函数参数
     *
     * @param paramElements 参数元素
     * @param index         索引位置
     * @param params        处理列表
     * @return 索引位置变更
     */
    private int handleFunctionParams(List<FormulaElement> paramElements, int index, List<Object> params) {
        int r = 0;
        FormulaElement ce = paramElements.get(index);
        if (ElementType.FUNCTION.equals(ce.getType()) || ElementType.BRACKETS.equals(ce.getType())) {
            params.add(handleCalcElement(Arrays.asList(ce)));
        } else if (ElementType.SUBTRACTION.equals(ce.getType())) {
            FormulaElement ne = paramElements.get(index + 1);
            if (ElementType.NUMBER.equals(ne.getType())) {
                r++;
                params.add(0 - Double.parseDouble(String.valueOf(ne.getContent())));
            }
        } else if (ElementType.NUMBER.equals(ce.getType())) {
            if (isNumber(ce.getContent().toString())) {
                params.add(new Double(String.valueOf(ce.getContent())));
            } else {
                params.add(ce.getContent());
            }
        } else if (ElementType.ARRAY.equals(ce.getType())) {
            Object[] os = (Object[]) ce.getContent();
            if (null != os && os.length > 0 && os[0] instanceof Double) {
                Double[] ds = new Double[os.length];
                for (int i = 0; i < os.length; i++) {
                    ds[i] = new Double(String.valueOf(os[i]));
                }
                params.add(ds);
            } else {
                params.add(ce.getContent());
            }
        } else if (ElementType.EXPRESSION.equals(ce.getType())) {
            List<FormulaElement> subElements = (List<FormulaElement>) ce.getContent();
            if (null != subElements && subElements.size() > 0) {
                Object o1 = handleCalcElement(Arrays.asList(subElements.get(0)));
                Object o2 = handleCalcElement(Arrays.asList(subElements.get(2)));
                FormulaElement expressionElement = subElements.get(1);
                if (ElementType.EQUAL.equals(expressionElement.getType())) {
                    if (o1 instanceof Boolean && o2 instanceof Double) {
                        o2 = FormulaUtil.toBool(o2);
                    } else if (o1 instanceof Double && o2 instanceof Boolean) {
                        o1 = FormulaUtil.toBool(o1);
                    }
                    params.add(o1.equals(o2));
                } else if (ElementType.NOT_EQUAL.equals(expressionElement.getType())) {
                    if (o1 instanceof Boolean && o2 instanceof Double) {
                        o2 = FormulaUtil.toBool(o2);
                    } else if (o1 instanceof Double && o2 instanceof Boolean) {
                        o1 = FormulaUtil.toBool(o1);
                    }
                    params.add(!o1.equals(o2));
                }
            }
        } else {
            params.add(ce.getContent());
        }
        return r;
    }

}



