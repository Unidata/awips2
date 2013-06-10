package com.raytheon.uf.common.registry.annotations;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import com.raytheon.uf.common.registry.RegistryManager;
import com.raytheon.uf.common.registry.constants.AssociationTypes;
import com.raytheon.uf.common.registry.ebxml.RegistryObjectResolver;

/**
 * An annotation that indicates that an association relationship exists between
 * two registry objects. The nature of that relationship is described by two
 * arguments: An AssociationResolver and an association type.
 * 
 * The AssociationResolver is used to extract the registry object(s) that
 * are/will be associated with the object marked with the
 * RegistryObjectAssociation annotation. The association type is used to define
 * the relationship between the objects.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Apr 17, 2012 455         jspinks     Initial creation
 * 4/9/2013     1802      bphillip   Using constant values from constants package instead of RegistryUtil
 * 
 * </pre>
 * 
 * @author jspinks
 * @version 1.0
 * @see RegistryManager {@link RegistryManager}
 * 
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.FIELD)
@Documented
public @interface RegistryObjectAssociation {
    public Class<? extends RegistryObjectResolver> value();

    public String associationType() default AssociationTypes.CONTAINS;
}
