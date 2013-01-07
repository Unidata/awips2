package com.raytheon.uf.common.registry.annotations;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import com.raytheon.uf.common.registry.RegistryManager;



/**
 * An annotation that indicates that an instance of this Class can be stored 
 * in the registry using the RegistryManager Class.  Member fields of a Class 
 * annotated with this annotation should identify the fields that should be
 * queryable with the @SlotAttribute and @SlotAttributeConverter annotations
 * as appropriate.
 * 
 * This annotation takes an optional list of fields names to use to generate the
 * RegistryObject id (and logical id) of the RegistryObject.  The values of each 
 * of the fields identified with be concatenated with a hyphen separator to produce
 * a String used to identify the Object in the registry.  If no field names are 
 * specified, a UUID will be used as the Id.
 * 
 * This annotation also takes an optional list of AssociationMapping definitions
 * to describe this Object's associations with other RegistryObjects. 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Mar 15, 2012             jspinks     Initial creation
 * May 15, 2012 455         jspinks     Updated to include AssociationMapping
 * 
 * </pre>
 * 
 * @author jspinks
 * @version 1.0
 * @see SlotAttribute {@link SlotAttribute}
 * @see SlotAttributeConverter {@link SlotAttributeConverter}
 * @see RegistryManager {@link RegistryManager}
 * @see AssociationMapping {@link AssociationMapping}
 * 
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
@Documented
public @interface RegistryObject {
    public Class<?> objectType() default Object.class;
    public String[] value() default {};
    public boolean storeContent() default true;
    public AssociationMapping[] associationMappings() default {};
}
