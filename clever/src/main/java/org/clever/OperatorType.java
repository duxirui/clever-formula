//package org.clever;
//
//import com.etransfar.tims3.common.exception.TimsException;
//import lombok.Getter;
//
///**
// * Description： 操作符类型
// * User: 杜涛
// * Date: Created in 2020/10/21 14:12
// * Version: 1.0.0
// * Modified By:
// */
//@Getter
//public enum OperatorType {
//
//    DATA("时间函数"),
//    LOGIC("逻辑函数"),
//    MATH("数学函数"),
//    TEXT("文本函数");
//
//    private String typeName;
//
//    OperatorType(String typeName) {
//        this.typeName = typeName;
//    }
//
//    public static OperatorType get(String name) {
//        for (OperatorType type : OperatorType.values()) {
//            if (type.toString().equals(name)) {
//                return type;
//            }
//        }
//        throw new TimsException("枚举名称非法");
//    }
//
//}
