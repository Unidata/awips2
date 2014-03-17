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

import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import org.hibernate.cfg.AnnotationConfiguration;
import org.hibernate.cfg.Configuration;
import org.hibernate.dialect.Dialect;
import org.springframework.orm.hibernate3.annotation.AnnotationSessionFactoryBean;

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
 * Jun 18, 2013 2117        djohnson    Remove use of config.buildSettings().
 * Oct 14, 2013 2361        njensen     Changes to support new technique for finding classes
 * 
 * 
 * </pre>
 * 
 */
public class DatabaseSessionFactoryBean extends AnnotationSessionFactoryBean {

    protected Class<?>[] accessibleClasses = null;

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
    public String[] getCreateSql(Set<Class<?>> classes)
            throws org.hibernate.AnnotationException {
        Configuration config = getConfiguration();
        AnnotationConfiguration tmp = loadNewConfigForClasses(classes);
        return tmp.generateSchemaCreationScript(Dialect.getDialect(config
                .getProperties()));
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
    public String[] getDropSql(Collection<Class<?>> classes)
            throws org.hibernate.AnnotationException {
        Configuration config = getConfiguration();
        AnnotationConfiguration tmp = loadNewConfigForClasses(classes);
        return tmp.generateDropSchemaScript(Dialect.getDialect(config
                .getProperties()));
    }

    private AnnotationConfiguration loadNewConfigForClasses(
            Collection<Class<?>> classes) {
        AnnotationConfiguration aConfig = new AnnotationConfiguration();

        for (Class<?> c : classes) {
            aConfig.addAnnotatedClass(c);
        }

        return aConfig;
    }

    public void setDatabaseSessionConfiguration(
            DatabaseSessionConfiguration databaseSessionConfiguration) {
        // make own copy so can modify it
        List<Class<?>> annotatedClasses = new LinkedList<Class<?>>(
                databaseSessionConfiguration.getAnnotatedClasses());

        if (databaseSessionConfiguration != null) {
            Iterator<Class<?>> iter = annotatedClasses.iterator();
            while (iter.hasNext()) {
                Class<?> clazz = iter.next();
                if (!databaseSessionConfiguration.matches(clazz.getName())) {
                    iter.remove();
                }
            }
        }

        // Set the annotated classes
        this.setAnnotatedClasses(annotatedClasses.toArray(new Class[] {}));
    }

    @SuppressWarnings("rawtypes")
    @Override
    public void setAnnotatedClasses(Class[] annotatedClasses) {
        super.setAnnotatedClasses(annotatedClasses);
        // overrode setter because we need access to the classes
        // for determining dependent classes for create/drop SQL
        this.accessibleClasses = annotatedClasses;
    }

    /**
     * Get the annotated classes associated with the database session
     * 
     * @return
     */
    public Class<?>[] getAnnotatedClasses() {
        return accessibleClasses;
    }

}
