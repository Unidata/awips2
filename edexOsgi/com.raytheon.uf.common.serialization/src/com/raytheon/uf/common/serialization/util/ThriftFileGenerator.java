package com.raytheon.uf.common.serialization.util;

import java.lang.reflect.Field;
import java.util.HashSet;
import java.util.Set;

import com.raytheon.uf.common.serialization.DynamicSerializationManager;
import com.raytheon.uf.common.serialization.DynamicSerializationManager.SerializationMetadata;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

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

/**
 * Generates a .thrift description file using annotations
 * 
 * This capability is currently a prototype.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 5, 2009            chammack     Initial creation
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class ThriftFileGenerator {

    private static Set<Class<?>> pendingClasses = new HashSet<Class<?>>();

    private static Set<Class<?>> completedClasses = new HashSet<Class<?>>();

    public static void main(String[] args) {
        if (args.length != 1) {
            System.out.println("Provide a class name to inspect");
            System.exit(0);
        }

        StringBuffer sb = new StringBuffer();
        Class<?> c = null;
        try {
            c = Class.forName(args[0]);
        } catch (ClassNotFoundException e1) {
            e1.printStackTrace();
            System.out.println("Class not found");
            System.exit(0);
        }

        buildThriftForClass(sb, c);

        while (pendingClasses.size() != 0) {
            for (Class<?> pending : new HashSet<Class<?>>(pendingClasses)) {
                buildThriftForClass(sb, pending);
                completedClasses.add(pending);
                pendingClasses.remove(pending);
            }
        }

        System.out.println(sb.toString());

    }

    private static void buildThriftForClass(StringBuffer sb, Class<?> c) {
        SerializationMetadata md = DynamicSerializationManager.inspect(c);

        if (md.serializationFactory != null) {
            System.out
                    .println(c.getName()
                            + ":: This class uses serialization factory.  Not currently supported");
            return;
        }

        sb.append("struct ");
        sb.append(c.getName().replace(".", "_"));
        sb.append(" {" + "\n");

        if (md.serializedAttributes != null) {
            int i = 1;
            for (String attribute : md.serializedAttributes) {
                try {
                    Field field = null;
                    Class<?> classToSearch = c;
                    do {
                        try {
                            field = classToSearch.getDeclaredField(attribute);
                        } catch (Exception e) {
                            classToSearch = classToSearch.getSuperclass();
                            if (classToSearch == null) {
                                System.out.println("Failed to find field: "
                                        + attribute);
                                System.exit(0);
                            }
                        }
                    } while (field == null);

                    Class<?> declaredClazz = field.getType();
                    String type = lookupType(declaredClazz);
                    if (type != null) {
                        if (i != 1) {
                            sb.append(",\n");
                        }
                        sb.append(i + ": " + type + " " + attribute);
                        i++;
                    } else {
                        System.out.println("Missing: " + attribute + ":: "
                                + declaredClazz);
                    }

                } catch (Exception e) {
                    e.printStackTrace();
                    System.out.println("Error generating schema for field");
                    System.exit(0);
                }

            }
            sb.append("\n}\n");

        }

    }

    private static String lookupType(Class<?> declaredClazz) {
        String type = null;
        if (declaredClazz == Double.class || declaredClazz == Double.TYPE) {
            type = "double";
        } else if (declaredClazz == String.class) {
            type = "string";
        } else if (declaredClazz == Boolean.class
                || declaredClazz == Boolean.TYPE) {
            type = "bool";
        } else if (declaredClazz == Byte.class || declaredClazz == Byte.TYPE) {
            type = "byte";
        } else if (declaredClazz == Integer.class
                || declaredClazz == Integer.TYPE) {
            type = "i32";
        } else if (declaredClazz == Short.class || declaredClazz == Short.TYPE) {
            type = "i16";
        } else if (declaredClazz == Long.class || declaredClazz == Long.TYPE) {
            type = "i64";
        } else if (declaredClazz.isArray()) {
            String subType = lookupType(declaredClazz.getComponentType());
            if (subType != null) {
                type = "list<" + subType + ">";
            }
        } else if (declaredClazz.getAnnotation(DynamicSerialize.class) != null) {
            type = declaredClazz.getName().replace(".", "_");
            if (!completedClasses.contains(declaredClazz)) {
                pendingClasses.add(declaredClazz);
            }
        }

        return type;
    }
}
