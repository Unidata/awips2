package com.raytheon.uf.common.registry.annotations;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * An annotation that indicates that when an instance of this Class is stored 
 * in the registry, a slotType should be created for the attribute marked with
 * this annotation.
 * 
 * Fields annotated in this fashion are assumed to has getters and setters for
 * the attribute.
 *  
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Mar 14, 2012             jspinks     Initial creation
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.FIELD)
@Documented
public @interface SlotAttribute {
    public String value() default "";
}
