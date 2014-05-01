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
package com.raytheon.uf.common.serialization.util;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Generates a python class and module that is equivalent to the Java class
 * passed in as an argument. This is meant to be used with DynamicSerialize to
 * generate the python side. Technically you could create the python object
 * definitions at runtime based on the decoding of the
 * SelfDescribingBinaryProtocol, but that could get messy.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 14, 2010            njensen     Initial creation
 * Jul 31, 2012  #965      dgilling    Fix path to file header.
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class PythonFileGenerator {

    private static final String NEW_LINE = "\n";

    private static final String INDENT = "    ";

    private static final String COMMENT = "# ";

    private static final String INIT_FILE = "__init__.py";

    public static void generateFile(File destDir, Class<?> clz, String header)
            throws IOException {
        String name = clz.getName();
        String shortname = name.substring(name.lastIndexOf('.') + 1);
        System.out.println(shortname);
        if (name.contains("$")) {
            System.out.println("Inner classes not supported");
            System.exit(0);
        }

        File parentFile = destDir;

        String[] packages = name.split("[.]");

        for (int i = 0; i < packages.length - 1; ++i) {
            File packageFile = new File(parentFile, packages[i]);
            if (packageFile.exists() && packageFile.isDirectory() == false) {
                packageFile.delete();
            }
            packageFile.mkdir();

            createInitFile(header, parentFile);

            parentFile = packageFile;
        }

        FileWriter fw = new FileWriter(new File(parentFile, shortname + ".py"),
                false);
        fw.write(header);
        fw.write(NEW_LINE);
        fw.write(COMMENT);
        fw.write("File auto-generated against equivalent DynamicSerialize Java class");
        fw.write(NEW_LINE);
        fw.write(NEW_LINE);
        fw.write("class " + shortname + "(object):");
        fw.write(NEW_LINE);
        fw.write(NEW_LINE);

        List<String> fields = getSerializedFields(clz);

        fw.write(INDENT);
        fw.write("def __init__(self):");
        fw.write(NEW_LINE);
        for (String s : fields) {
            fw.write(INDENT);
            fw.write(INDENT);
            fw.write("self.");
            fw.write(s);
            fw.write(" = None");
            fw.write(NEW_LINE);
        }

        fw.write(NEW_LINE);
        for (String s : fields) {
            String title = s.substring(0, 1).toUpperCase() + s.substring(1);
            fw.write(INDENT);
            fw.write("def get");
            fw.write(title);
            fw.write("(self):");
            fw.write(NEW_LINE);
            fw.write(INDENT);
            fw.write(INDENT);
            fw.write("return self.");
            fw.write(s);
            fw.write(NEW_LINE);
            fw.write(NEW_LINE);

            fw.write(INDENT);
            fw.write("def set");
            fw.write(title);
            fw.write("(self, ");
            fw.write(s);
            fw.write("):");
            fw.write(NEW_LINE);
            fw.write(INDENT);
            fw.write(INDENT);
            fw.write("self.");
            fw.write(s);
            fw.write(" = ");
            fw.write(s);
            fw.write(NEW_LINE);
            fw.write(NEW_LINE);
        }

        fw.flush();
        fw.close();

        createInitFile(header, parentFile);
    }

    private static void createInitFile(String header, File dir)
            throws IOException {
        File initFile = new File(dir, "__init__.py");
        initFile.delete();
        FileWriter fw = new FileWriter(initFile, false);
        fw.write(header);
        fw.write(NEW_LINE);
        fw.write(COMMENT);
        fw.write("File auto-generated by PythonFileGenerator");
        fw.write(NEW_LINE);
        fw.write(NEW_LINE);
        fw.write("__all__ = [");
        fw.write(NEW_LINE);

        File[] files = dir.listFiles();
        List<String> dirs = new ArrayList<String>();
        List<String> pythonFiles = new ArrayList<String>();
        // add all packages, then files
        for (int i = 0; i < 2; ++i) {
            for (File file : files) {
                String fileName = file.getName();
                if (i == 0 && file.isDirectory() && file.isHidden() == false) {
                    dirs.add(fileName);
                } else if (i == 1 && fileName.endsWith(".py")
                        && fileName.equals(INIT_FILE) == false) {
                    dirs.add(fileName.substring(0, fileName.length() - 3));
                    pythonFiles
                            .add(fileName.substring(0, fileName.length() - 3));
                }
            }
        }

        Collections.sort(dirs);
        Collections.sort(pythonFiles);

        for (int j = 0; j < dirs.size(); ++j) {
            fw.write(INDENT + INDENT + INDENT);
            fw.write("'");
            fw.write(dirs.get(j));
            fw.write("'");
            if (j < dirs.size() - 1) {
                fw.write(",");
            }
            fw.write(NEW_LINE);
        }

        fw.write("          ]");
        fw.write(NEW_LINE);
        fw.write(NEW_LINE);

        for (String pythonFile : pythonFiles) {
            fw.write("from " + pythonFile + " import " + pythonFile);
            fw.write(NEW_LINE);
        }
        fw.write(NEW_LINE);

        fw.flush();
        fw.close();
    }

    public static List<String> getSerializedFields(Class<?> clz) {
        List<String> list = new ArrayList<String>();
        while (clz != null) {
            Field[] fields = clz.getDeclaredFields();
            for (Field f : fields) {
                Object ann = f.getAnnotation(DynamicSerializeElement.class);
                if (ann != null) {
                    list.add(f.getName());
                }
            }
            clz = clz.getSuperclass();
        }

        return list;
    }

    /**
     * @param args
     */
    public static void main(String[] args) throws Exception {
        String fileToRead = null;
        String destDir = null;

        for (int i = 0; i < args.length; ++i) {
            if (args[i].equals("-f") && i < args.length - 1) {
                fileToRead = args[++i];
            } else if (args[i].equals("-d") && i < args.length - 1) {
                destDir = args[++i];
            }
        }

        if (fileToRead == null) {
            System.err
                    .println("Pass in file to read classes in from using -f <filepath> argument");
            System.exit(1);
        }

        File readFile = new File(fileToRead);
        File destFile = null;

        if (destDir == null) {
            System.out
                    .println("No destination directory specified, specify with -d <dir> argument");

            File tmp = File.createTempFile("tmp", "");
            destFile = new File(tmp.getParentFile(), "python");
            tmp.delete();

            if (destFile.exists() && destFile.isDirectory() == false) {
                destFile.delete();
            }
            destFile.mkdirs();
        } else {
            destFile = new File(destDir);
            if (destFile.exists() == true && destFile.isDirectory() == false) {
                System.err
                        .println("Can not write to destination directory, it is already a file");
                System.exit(2);
            }
            destFile.mkdirs();

        }

        System.out.println("Reading class list file: "
                + readFile.getAbsolutePath());
        System.out.println("Writing python file to "
                + destFile.getAbsolutePath());

        try {
            BufferedReader br = new BufferedReader(new FileReader(readFile));
            String line;
            while ((line = br.readLine()) != null) {
                if ("".equals(line.trim())) {
                    continue;
                }
                Class<?> c = null;
                try {
                    c = Class.forName(line);
                } catch (ClassNotFoundException e1) {
                    e1.printStackTrace();
                    System.out.println("Class not found: " + line);
                    continue;
                }

                String header = null;
                try {
                    header = getHeaderInfo();
                } catch (IOException e) {
                    e.printStackTrace();
                    System.exit(0);
                }

                try {
                    generateFile(destFile, c, header);
                } catch (IOException e) {
                    e.printStackTrace();
                    continue;
                }
            }
        } catch (FileNotFoundException e2) {
            e2.printStackTrace();
        }

        System.out.println("Classes generated");
    }

    public static String getHeaderInfo() throws IOException {
        File file = new File(
                "../../cave/build/tools/headup/AWIPS/awipsHeader.txt");
        if (!file.exists()) {
            System.out
                    .println("Unable to determine header information, skipping header");
            return "";
        }

        BufferedReader br = new BufferedReader(new FileReader(file));
        StringBuffer sb = new StringBuffer();
        sb.append("##" + NEW_LINE);
        String line = br.readLine();
        while (line != null) {
            sb.append(COMMENT);
            sb.append(line);
            sb.append(NEW_LINE);
            line = br.readLine();
        }
        sb.append("##" + NEW_LINE);
        br.close();

        return sb.toString();
    }
}
