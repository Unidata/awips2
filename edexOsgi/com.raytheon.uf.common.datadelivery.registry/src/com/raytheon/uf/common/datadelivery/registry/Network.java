package com.raytheon.uf.common.datadelivery.registry;

import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlType;

/**
 * An enumeration of the possible values of the route attribute.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 01, 2012 726        jspinks     Initial creation
 * Nov 09, 2012 1286       djohnson    Remove ENTERPRISE.
 * Nov 19, 2012 1166       djohnson    Clean up JAXB representation of registry objects.
 * 
 * </pre>
 * 
 * @version 1.0
 */
@XmlEnum
@XmlType(name = "Network", namespace = "bandwidth")
public enum Network {
    OPSNET,
    SBN
}