package com.raytheon.uf.viz.datadelivery.utils;

import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlEnumValue;
import javax.xml.bind.annotation.XmlType;

import com.raytheon.uf.viz.datadelivery.system.Operator;

/** Enumeration to use for Datatype operations */
@XmlType(name = "typeOperationItems")
@XmlEnum
public enum TypeOperationItems implements Operator<String> {
    /** Operation IN */
    @XmlEnumValue("IN")
    IN("IN"),
    /** Operation NOT IN */
    @XmlEnumValue("NOT IN")
    NOT_IN("NOT IN");

    /** Datatype operation */
    private final String typeOperation;

    private TypeOperationItems(String typeOperation) {
        this.typeOperation = typeOperation;
    }

    /**
     * Get datatype operation.
     * 
     * @return typeOperation
     */
    public String getOperation() {
        return typeOperation;
    }

    @Override
    public String toString() {
        return typeOperation;
    }

    @Override
    public boolean evaluate(String operandOne, String operandTwo) {
        if (TypeOperationItems.IN == this) {
            if (operandOne.toLowerCase().contains(operandTwo.toLowerCase())) {
                return true;
            }
        } else if (TypeOperationItems.NOT_IN == this) {
            if (!operandOne.contains(operandTwo)) {
                return true;
            }
        }

        return false;
    }
}