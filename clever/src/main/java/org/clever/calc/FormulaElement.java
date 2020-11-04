package org.clever.calc;

import org.clever.enums.ElementType;
import org.clever.exception.FormulaParserException;

import java.util.*;

/**
 * Description：公式计算元素
 * User: 沐彦
 * Date: Created in 2020/10/6 09:15
 * Version: 1.0.0
 * Modified By:
 */
class FormulaElement {

    private ElementType type;

    private Object content;

    private List<FormulaElement> params;

    public ElementType getType() {
        return this.type;
    }

    public Object getContent() {
        return this.content;
    }

    public List<FormulaElement> getParams() {
        return this.params;
    }

    public static FormulaElement getInstance(ElementType type, Object content) {
        return getInstance(type, content, null);
    }

    public void setType(ElementType type) {
        this.type = type;
    }

    public void setContent(Object content) {
        this.content = content;
    }

    public static FormulaElement getInstance(ElementType type, Object content, List<FormulaElement> params) {
        if (Objects.isNull(type) || (Objects.isNull(content)) && "".equals(content))
            throw new FormulaParserException("Illegal content of formula parsing element!");
        FormulaElement formulaElement = new FormulaElement();
        formulaElement.type = type;
        formulaElement.content = content;
        if ((ElementType.BRACKETS.equals(type) || ElementType.FUNCTION.equals(type))) {
            formulaElement.params = new ArrayList<>();
            if (params instanceof List && !((List) params).isEmpty()) {
                for (Object o : ((List) params)) {
                    if (o instanceof FormulaElement) {
                        formulaElement.params.add((FormulaElement) o);
                    } else {
                        throw new FormulaParserException("Illegal content of formula parsing element!");
                    }
                }
            }
        }
        return formulaElement;
    }

}
