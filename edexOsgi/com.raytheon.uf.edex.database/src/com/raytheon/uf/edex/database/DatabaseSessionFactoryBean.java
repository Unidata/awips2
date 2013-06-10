/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/

package com.raytheon.uf.edex.database;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import org.hibernate.cfg.AnnotationConfiguration;
import org.hibernate.cfg.Configuration;
import org.hibernate.dialect.Dialect;
import org.springframework.orm.hibernate3.annotation.AnnotationSessionFactoryBean;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.SerializableManager;

/**
 * Extension of the AnnotationSessionFactoryBean provided by Spring.
 * <p>
 * This class utilizes the SerializableManager to dynamically discover which
 * classes are mapped using annotations. The existing
 * AnnotationSessionFactoryBean requires a list of annotated classes be provided
 * in the Hibernate configuration. This approach is inadequate for the software
 * architecture.
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 10/8/2008    1532        bphillip    Initial checkin
 * 
 * </pre>
 * 
 */
public class DatabaseSessionFactoryBean extends AnnotationSessionFactoryBean {
    /**
     * Creates a new MetadataSessionFactoryBean.
     * <p>
     * This constructor uses the SerializableManager to dynamically discover and
     * provide the underlying Hibernate SessionFactory with a list of mapped
     * classes
     */
    public DatabaseSessionFactoryBean() {
        super();
    }

    /**
     * Generates the create ddl for the passed in hibernate annotated pojos. All
     * relationships need to be available in the passed in array of classes.
     * i.e. if a class has a OneToMany the subclass will also need to be in the
     * passed in array.
     * 
     * @param classes
     *            Classes to generate the ddl for.
     * @return List of create sql.
     * @throws org.hibernate.AnnotationException
     */
    public String[] getCreateSql(Set<Class<ISerializableObject>> classes)
            throws org.hibernate.AnnotationException {
        Configuration config = getConfiguration();
        AnnotationConfiguration tmp = loadNewConfigForClasses(classes);
        return tmp.generateSchemaCreationScript(config.buildSettings().getDialect());
    }

    /**
     * Generates the drop ddl for the passed in hibernate annotated pojos. All
     * relationships need to be available in the passed in array of classes.
     * i.e. if a class has a OneToMany the subclass will also need to be in the
     * passed in array.
     * 
     * @param classes
     *            Classes to generate the ddl for.
     * @return List of create sql.
     * @throws org.hibernate.AnnotationException
     */
    public String[] getDropSql(Collection<Class<ISerializableObject>> classes)
            throws org.hibernate.AnnotationException {
        Configuration config = getConfiguration();
        AnnotationConfiguration tmp = loadNewConfigForClasses(classes);
        return tmp.generateDropSchemaScript(config.buildSettings().getDialect());
    }

    private AnnotationConfiguration loadNewConfigForClasses(
            Collection<Class<ISerializableObject>> classes) {
        AnnotationConfiguration aConfig = new AnnotationConfiguration();

        for (Class<ISerializableObject> c : classes) {
            aConfig.addAnnotatedClass(c);
        }

        return aConfig;
    }

    public void setDatabaseSessionConfiguration(
            DatabaseSessionConfiguration databaseSessionConfiguration) {
        SerializableManager serialManager = SerializableManager.getInstance();

        // make own copy so can modify it
        List<String> pluginFQNs = new ArrayList<String>(serialManager
                .getHibernatablePluginFQNs());

        if (databaseSessionConfiguration != null) {
            Iterator<String> iter = pluginFQNs.iterator();
            while (iter.hasNext()) {
                String fqn = iter.next();
                if (!databaseSessionConfiguration.matches(fqn)) {
                    iter.remove();
                }
            }
        }

        if (pluginFQNs != null && pluginFQNs.size() > 0) {
            // Get the lists of annotated classes
            List<Class<ISerializableObject>> annotatedClasses = new ArrayList<Class<ISerializableObject>>(
                    10 * pluginFQNs.size());

            for (String fqn : pluginFQNs) {
                annotatedClasses.addAll(serialManager
                        .getHibernatablesForPluginFQN(fqn));
            }

            // Set the annotated classes
            this.setAnnotatedClasses(annotatedClasses.toArray(new Class[] {}));
        }
    }
}