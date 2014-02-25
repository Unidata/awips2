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
package com.raytheon.uf.edex.archive.feeder;

import java.io.File;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.persistence.ManyToOne;

import org.apache.commons.beanutils.PropertyUtils;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.core.EdexException;
import com.raytheon.uf.edex.database.plugin.PluginDao;
import com.raytheon.uf.edex.database.plugin.PluginFactory;

/**
 * This class used to process binary files created for the archive directory.
 * Not intended for production. Designed for testing of problems with creating
 * archive data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 25, 2014 2838       rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */

public class ArchiveBinaryFeederIngester {

    protected static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(ArchiveBinaryFeederIngester.class);

    /** The ManyToOne fields in a given class. */
    private final Map<Class<?>, Field[]> manyToOneFieldMap = new HashMap<Class<?>, Field[]>();

    /**
     * The constructor. if needed attempts to created the drop directory
     */
    public ArchiveBinaryFeederIngester() {
        String rootName = System.getProperty("archive.feeder.directory");
        if (rootName != null) {
            File rootDir = new File(rootName);
            if (rootDir.exists()) {
                if (!rootDir.isDirectory()) {
                    if (rootDir.delete()) {
                        rootDir.mkdirs();
                    }
                }
            } else {
                rootDir.mkdirs();
            }
        }
    }

    /**
     * Process archive binary file.
     * 
     * @param file
     * @throws EdexException
     */
    public void processEvent(File file) throws EdexException {
        IUFStatusHandler statusHandler = UFStatus
                .getHandler(ArchiveBinaryFeederIngester.class);

        boolean enabled = Boolean.getBoolean("archive.feeder.enable");
        if (!enabled) {
            if (statusHandler.isPriorityEnabled(Priority.WARN)) {
                StringBuilder sb = new StringBuilder(
                        ArchiveBinaryFeederIngester.class.getName());
                sb.setLength(sb.lastIndexOf("."));
                statusHandler
                        .handle(Priority.WARN,
                                String.format(
                                        "file \"%s\" not processed; feeder not enabled consider removing plugin: %s",
                                        file, sb.toString()));
            }
            return;
        }

        long start = System.currentTimeMillis();
        ArchiveBinaryReader reader = new ArchiveBinaryReader(file);

        try {
            if (statusHandler.isPriorityEnabled(Priority.INFO)) {
                statusHandler.handle(
                        Priority.INFO,
                        String.format("Start processing file \"%1$s\".",
                                file.getAbsoluteFile()));
            }

            Object object = reader.getObject();
            if (!(object instanceof List<?>)) {
                if (statusHandler.isPriorityEnabled(Priority.INFO)) {
                    statusHandler
                            .info(String
                                    .format("file: \"%s\" does not contain a list of PluginDataObjects",
                                            file.getAbsolutePath()));
                }
            } else {
                List<?> objects = (List<?>) object;
                process(objects);
            }
        } catch (Exception e) {
            if (statusHandler.isPriorityEnabled(Priority.PROBLEM)) {
                statusHandler.handle(Priority.PROBLEM, String.format(
                        "Problem processing the file: \"%s\", %s",
                        file.getAbsolutePath(), e.getLocalizedMessage()));
            }

        } finally {
            if (statusHandler.isPriorityEnabled(Priority.INFO)) {
                long time = System.currentTimeMillis() - start;
                statusHandler.handle(Priority.INFO, String.format(
                        "Finished processing file \"%1$s\" in %2$d msec.",
                        file.getAbsoluteFile(), time));
            }
        }
    }

    /**
     * Checks to see if the objects is a list of PluginDataObjects and attempts
     * use the proper dao to send list to the data base.
     * 
     * @param objects
     * @throws PluginException
     */
    private void process(List<?> objects) throws PluginException {
        if (objects.size() > 0) {
            Object o = objects.iterator().next();

            if (o instanceof PluginDataObject) {
                PluginDataObject pdo = (PluginDataObject) o;
                String pdoName = pdo.getPluginName();

                PluginDao dao = PluginFactory.getInstance().getPluginDao(
                        pdoName);

                processManytoOneFields(objects, pdo.getClass(), dao);

                dao.persistAll(objects);
            } else {
                throw new PluginException("Not a list of PluginDataObjects.");
            }
        }
    }

    /**
     * Determine the fields in the class that are an instance of the ManyToOne
     * class. This will always return a non null result.
     * 
     * @param clazz
     * @return fields
     */
    private synchronized Field[] getManyToOneFields(Class<?> clazz) {
        Field[] fields = manyToOneFieldMap.get(clazz);
        if (fields == null) {
            List<Field> fieldList = new ArrayList<Field>();
            Class<?> currentClass = clazz;

            while (currentClass != null) {
                for (Field field : currentClass.getDeclaredFields()) {
                    if (field.getAnnotation(ManyToOne.class) != null) {
                        fieldList.add(field);
                    }
                }
                currentClass = currentClass.getSuperclass();
            }

            fields = fieldList.toArray(new Field[fieldList.size()]);
            manyToOneFieldMap.put(clazz, fields);
        }

        return fields;
    }

    /**
     * Process the Many to one fields of a class so their values are in the data
     * base prior to attempting to place the objects in the data base.
     * 
     * @param objects
     * @param clazz
     */
    private void processManytoOneFields(Collection<?> objects, Class<?> clazz,
            PluginDao dao) {
        Field[] fields = getManyToOneFields(clazz);
        if (fields.length > 0) {
            Map<Object, Object> foMap = new HashMap<Object, Object>();
            for (Field field : fields) {
                for (Object object : objects) {
                    try {
                        Object fieldObject = PropertyUtils.getProperty(object,
                                field.getName());
                        if (fieldObject != null) {
                            Object fo = foMap.get(fieldObject);
                            if (fo == null) {
                                foMap.put(fieldObject, fieldObject);
                            } else {
                                // Set to the instance being sent to the dao.
                                PropertyUtils.setProperty(object,
                                        field.getName(), fo);
                            }
                        }
                    } catch (Exception e) {
                        statusHandler.handle(Priority.PROBLEM,
                                e.getLocalizedMessage(), e);
                    }
                }

                Collection<?> fieldObjects = foMap.values();
                processManytoOneFields(fieldObjects, field.getType(), dao);
                dao.persistAll(fieldObjects);
            }
        }
    }
}
