/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 *
 */
package org.apache.qpid.gentools;

import org.apache.velocity.Template;
import org.apache.velocity.VelocityContext;
import org.apache.velocity.app.Velocity;
import org.w3c.dom.Node;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.LineNumberReader;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.EnumMap;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.Map;

public abstract class Generator implements LanguageConverter
{
    protected static String CR = Utils.LINE_SEPARATOR;


    private static final Map<String, Integer> FIXED_SIZE_TYPES = new HashMap<String, Integer>();

    static
    {
        FIXED_SIZE_TYPES.put("bit", 1);
        FIXED_SIZE_TYPES.put("bitfield", 1);
        FIXED_SIZE_TYPES.put("long", 4);
        FIXED_SIZE_TYPES.put("longlong", 8);
        FIXED_SIZE_TYPES.put("octet", 1);
        FIXED_SIZE_TYPES.put("short", 2);
        FIXED_SIZE_TYPES.put("timestamp", 8);

    }

    private String _templateDirectory;
    private String _outputDirectory;

    public AmqpDomainMap getDomainMap()
    {
        return _domainMap;
    }

    public AmqpConstantSet getConstantSet()
    {
        return _constantSet;
    }

    public AmqpModel getModel()
    {
        return _model;
    }

    abstract public String getNativeType(String type);

    abstract public String getEncodingType(String type);



    protected static enum EnumConstOutputTypes
    {
        OUTPUT_STRING,
        OUTPUT_INTEGER,
        OUTPUT_DOUBLE;
    }

    ;

    public static enum TemplateType
    {
        model("model"),
        clazz("class"),
        method("method"),
        field("field");

        private final String _name;

        private TemplateType(String name)
        {
            _name = name;
        }

        public String getName()
        {
            return _name;
        }
    }

    ;


    public static interface Factory<X extends Generator>
    {
        public X newInstance();
    }


    protected static final class NamedTemplate
    {
        private final String _name;
        private final String _template;
        private final File _file;


        public NamedTemplate(String relativePath, File templateFile)
        {
            _file = templateFile;
            _name = relativePath + Utils.FILE_SEPARATOR + templateFile.getName();

            _template = loadTemplate(templateFile);
        }


        public String getName()
        {
            return _name;
        }

        public String getTemplate()
        {
            return _template;
        }


        public File getFile()
        {
            return _file;
        }

    }


    private static final String VELOCITY_TEMPLATE_SUFFIX = ".vm";
    private static final String STANDARD_TEMPLATE_SUFFIX = ".tmpl";
    private static FilenameFilter _tmplFileFilter = new FilenameFilter()
    {

        public boolean accept(File dir, String name)
        {
            return name.endsWith(STANDARD_TEMPLATE_SUFFIX) || name.endsWith(VELOCITY_TEMPLATE_SUFFIX);
        }
    };


    // This string is reproduced in every generated file as a comment
    // TODO: Tie the version info into the build system.
    protected static final String GENERATOR_INFO = "Qpid Gentools v.0.1";


    private final Map<TemplateType, Collection<NamedTemplate>> _templates =
            new EnumMap<TemplateType, Collection<NamedTemplate>>(TemplateType.class);

    private final Map<TemplateType, Collection<NamedTemplate>> _versionSpecificTemplates =
            new EnumMap<TemplateType, Collection<NamedTemplate>>(TemplateType.class);


    private final AmqpVersionSet _versionSet;

    private final AmqpDomainMap _domainMap;
    private final Map<AmqpVersion, AmqpDomainMap> _versionToDomainMapMap = new HashMap<AmqpVersion, AmqpDomainMap>();

    private final AmqpConstantSet _constantSet;
    private final Map<AmqpVersion, AmqpConstantSet> _versionToConstantSetMap = new HashMap<AmqpVersion, AmqpConstantSet>();


    public AmqpVersionSet getVersionSet()
    {
        return _versionSet;
    }

    private final AmqpModel _model;
    private final Map<AmqpVersion, AmqpModel> _versionToModelMap = new HashMap<AmqpVersion, AmqpModel>();

    protected int generatedFileCounter;

    public Generator()
    {
        _versionSet = new AmqpVersionSet();
        _model = new AmqpModel(this);
        _constantSet = new AmqpConstantSet(this);
        _domainMap = new AmqpDomainMap(this);

        generatedFileCounter = 0;
    }

//    public final AmqpVersionSet getVersionSet()
//    {
//        return _versionSet;
//    }


    public void addVersion(AmqpVersion version)
    {
        _versionSet.add(version);
        if (!_versionToModelMap.containsKey(version))
        {
            _versionToModelMap.put(version, new AmqpModel(this));
        }
        if (!_versionToDomainMapMap.containsKey(version))
        {
            _versionToDomainMapMap.put(version, new AmqpDomainMap(this));
        }
        if (!_versionToConstantSetMap.containsKey(version))
        {
            _versionToConstantSetMap.put(version, new AmqpConstantSet(this));
        }
    }

    public int getNumberGeneratedFiles()
    {
        return generatedFileCounter;
    }

//	public AmqpDomainMap getDomainMap()
//	{
//		return _domainMap;
//	}
//
//    public AmqpConstantSet getConstantSet()
//    {
//        return _constantSet;
//    }
//
//
//	public AmqpModel getModel()
//	{
//		return _model;
//	}

    public void initializeTemplates() throws IOException
    {

        for (TemplateType type : EnumSet.allOf(TemplateType.class))
        {
            ArrayList<NamedTemplate> typeTemplates = new ArrayList<NamedTemplate>();
            _templates.put(type, typeTemplates);
            ArrayList<NamedTemplate> versionSpecificTypeTemplates = new ArrayList<NamedTemplate>();
            _versionSpecificTemplates.put(type, versionSpecificTypeTemplates);

            File templateDirectory = new File(getTemplateDirectory() + Utils.FILE_SEPARATOR + type.getName());
            File versionTemplateDirectory = new File(getTemplateDirectory() + Utils.FILE_SEPARATOR + type.getName() + Utils.FILE_SEPARATOR + "version");

            System.out.println("Looking for template files in directory: " + templateDirectory.getAbsoluteFile());

            File[] templateFiles = templateDirectory.listFiles(_tmplFileFilter);

            File[] versionTemplateFiles = new File[0];

            System.out.println("Looking for version specific template files in directory: " + versionTemplateDirectory.getAbsoluteFile());

            if (versionTemplateDirectory.exists())
            {
                versionTemplateFiles = versionTemplateDirectory.listFiles(_tmplFileFilter);
            }

			if(templateFiles != null)
			{
	            for (File templateFile : templateFiles)
	            {
	                System.out.println(type.getName() + " template file(s):");
	                System.out.println("  " + templateFile.getCanonicalPath());
	                typeTemplates.add(new NamedTemplate(type.getName(), templateFile));
	            }
			}

            if(versionTemplateFiles != null)
			{
				for (File versionTemplateFile : versionTemplateFiles)
	            {
	                System.out.println(type.getName() + " template file(s):");
	                System.out.println("  " + versionTemplateFile.getCanonicalPath());
	                versionSpecificTypeTemplates.add(new NamedTemplate(type.getName() + Utils.FILE_SEPARATOR + "version", versionTemplateFile));
	            }
			}

        }
    }

    public String getTemplateDirectory()
    {
        return _templateDirectory;
    }


    public void setTemplateDirectory(String templateDirectory)
    {
        _templateDirectory = templateDirectory;
    }


    public void setOutputDirectory(String outputDirectory)
    {
        _outputDirectory = outputDirectory;
    }

    public void generate()
    {
        prepareTargetDirectory(new File(_outputDirectory), true);
        System.out.println("Generation directory: " + _outputDirectory);


        processModelTemplates(_templates);

        for (AmqpClass amqpClass : _model.getClassMap().values())
        {
            processClassTemplates(_templates, amqpClass);

            for (AmqpMethod amqpMethod : amqpClass.getMethodMap().values())
            {
                processMethodTemplates(_templates, amqpClass, amqpMethod);

                for (AmqpField amqpField : amqpMethod.getFieldMap().values())
                {
                    processFieldTemplates(_templates, amqpClass, amqpMethod, amqpField, null);
                }
            }
        }


        for (AmqpVersion version : _versionSet)
        {
            AmqpModel model = _versionToModelMap.get(version);
            processModelTemplates(_versionSpecificTemplates, version);

            for (AmqpClass amqpClass : model.getClassMap().values())
            {
                processClassTemplates(_versionSpecificTemplates, amqpClass, version);

                for (AmqpMethod amqpMethod : amqpClass.getMethodMap().values())
                {
                    processMethodTemplates(_versionSpecificTemplates, amqpClass, amqpMethod, version);

                    for (AmqpField amqpField : amqpMethod.getFieldMap().values())
                    {
                        processFieldTemplates(_versionSpecificTemplates, amqpClass, amqpMethod, amqpField, version);
                    }
                }
            }

        }
    }

    private void processMethodTemplates(Map<TemplateType, Collection<NamedTemplate>> templates, AmqpClass amqpClass, AmqpMethod amqpMethod, AmqpVersion version)
    {
        for (NamedTemplate template : templates.get(TemplateType.method))
        {
            if(isVelocityTemplate(template))
            {
                processVelocityTemplate(template,version,amqpClass,amqpMethod,null);
            }
            else
            {
                processMethodTemplate(template, amqpClass, amqpMethod);
            }
        }
        
    }

    private void processClassTemplates(Map<TemplateType, Collection<NamedTemplate>> templates, AmqpClass amqpClass, AmqpVersion version)
    {
        for (NamedTemplate template : templates.get(TemplateType.clazz))
        {
            if(isVelocityTemplate(template))
            {
                processVelocityTemplate(template,version,amqpClass,null,null);
            }
            else
            {
                processClassTemplate(template, amqpClass);
            }
        }

    }


    private void processModelTemplates(Map<TemplateType, Collection<NamedTemplate>> templates, AmqpVersion version)
    {
        for (NamedTemplate template : templates.get(TemplateType.model))
        {
            if (isVelocityTemplate(template))
            {
                processModelVelocityTemplate(template, version);
            }
            else
            {
                processModelTemplate(template, version);
            }
        }
    }

    abstract void processModelTemplate(NamedTemplate template, AmqpVersion version);


    protected void processModelTemplates(Map<TemplateType, Collection<NamedTemplate>> templates)
    {
        for (NamedTemplate template : templates.get(TemplateType.model))
        {
            if (isVelocityTemplate(template))
            {
                processModelVelocityTemplate(template, null);
            }
            else
            {
                processModelTemplate(template);
            }
        }
    }

    private boolean isVelocityTemplate(NamedTemplate template)
    {
        return template.getName().endsWith(VELOCITY_TEMPLATE_SUFFIX);
    }

    private void processModelVelocityTemplate(NamedTemplate template, AmqpVersion version)
    {
        processVelocityTemplate(template,version,null,null,null);
    }

    private void processVelocityTemplate(NamedTemplate template, AmqpVersion version,
                                              AmqpClass amqpClass, AmqpMethod amqpMethod, AmqpField amqpField)
    {

        VelocityContext context = new VelocityContext();

        AmqpModel model = _model;
        if(version != null)
        {
            model = _versionToModelMap.get(version);
        }
        context.put("model", model);
        context.put("generator", GENERATOR_INFO);

        if (version != null)
        {
            context.put("version", version);
        }
        if(amqpClass != null)
        {
            context.put("amqpClass", amqpClass);
        }

        if(amqpClass != null)
        {
            context.put("amqpMethod", amqpMethod);
        }


        StringWriter sw = new StringWriter();


        try
        {
            Template velocityTemplate = Velocity.getTemplate(template.getName());
            velocityTemplate.merge(context, sw);
            String filename = String.valueOf(context.get("filename"));

            File outputFile = new File(getOutputDirectory() + Utils.FILE_SEPARATOR + filename);
            outputFile.getParentFile().mkdirs();
            FileWriter outputFileWriter = new FileWriter(outputFile);

            outputFileWriter.append(sw.toString());
            outputFileWriter.close();

        }
        catch (Exception e)
        {
            e.printStackTrace();
        }


    }


    protected void processClassTemplates(Map<TemplateType, Collection<NamedTemplate>> templates, AmqpClass amqpClass)
    {
        for (NamedTemplate template : templates.get(TemplateType.clazz))
        {
            if(isVelocityTemplate(template))
            {
                processVelocityTemplate(template,null,amqpClass,null,null);
            }
            else
            {
                processClassTemplate(template, amqpClass);
            }
        }
    }

    protected void processMethodTemplates(Map<TemplateType, Collection<NamedTemplate>> templates, AmqpClass amqpClass, AmqpMethod amqpMethod)
    {
        for (NamedTemplate template : templates.get(TemplateType.method))
        {
            if(isVelocityTemplate(template))
            {
                processVelocityTemplate(template,null,amqpClass,amqpMethod,null);
            }
            else
            {
                processMethodTemplate(template, amqpClass, amqpMethod);
            }
        }
    }


    protected void processFieldTemplates(Map<TemplateType, Collection<NamedTemplate>> templates, AmqpClass amqpClass, AmqpMethod amqpMethod, AmqpField amqpField, AmqpVersion amqpVersion)
    {
        for (NamedTemplate template : templates.get(TemplateType.field))
        {
            if(isVelocityTemplate(template))
            {
                processVelocityTemplate(template,amqpVersion,amqpClass,amqpMethod,amqpField);
            }
            else
            {
                processTemplate(template, amqpClass, amqpMethod, amqpField, amqpVersion);
            }
        }
    }


    protected void processVersionList(StringBuffer sb, int tokStart, int tokEnd)
    {
        int lend = sb.indexOf(Utils.LINE_SEPARATOR, tokStart) + 1; // Include cr at end of line
        String tline = sb.substring(tokEnd, lend); // Line excluding line marker, including cr
        sb.delete(tokStart, lend);

        for (AmqpVersion v : _versionSet)
        {
            // Insert copy of target line
            StringBuffer isb = new StringBuffer(tline);
            if (isb.indexOf("${protocol-version-list-entry}") >= 0)
            {
                String versionListEntry = "       { ${major}, ${minor} }" +
                                          (v.equals(_versionSet.last()) ? "" : ",");
                replaceToken(isb, "${protocol-version-list-entry}", String.valueOf(versionListEntry));
            }
            if (isb.indexOf("${major}") >= 0)
            {
                replaceToken(isb, "${major}", String.valueOf(v.getMajor()));
            }
            if (isb.indexOf("${minor}") >= 0)
            {
                replaceToken(isb, "${minor}", String.valueOf(v.getMinor()));
            }
            sb.insert(tokStart, isb.toString());
            tokStart += isb.length();
        }
    }

    // Helper functions common to all generators

    protected static void prepareTargetDirectory(File dir, boolean createFlag)
    {
        if (dir.exists())
        {
            if (!dir.isDirectory())
            {
                throw new TargetDirectoryException("\"" + dir.getAbsolutePath() +
                                                   "\" exists, but is not a directory.");
            }
        }
        else if (createFlag) // Create dir
        {
            if (!dir.mkdirs())
            {
                throw new TargetDirectoryException("Unable to create directory \"" +
                                                   dir.getAbsolutePath() + "\".");
            }
        }
        else
        {
            throw new TargetDirectoryException("Directory \"" + dir.getAbsolutePath() +
                                               "\" not found.");
        }

    }

    protected void processAllLists(StringBuffer sb, AmqpClass thisClass, AmqpMethod method, AmqpVersion version)
    {
        AmqpModel model = (version == null) ? _model : _versionToModelMap.get(version);


        int lstart = sb.indexOf("%{");
        while (lstart != -1)
        {
            int lend = sb.indexOf("}", lstart + 2);
            if (lend > 0)
            {
                String listToken = sb.substring(lstart + 2, lend);
                if (listToken.compareTo("VLIST") == 0)
                {
                    processVersionList(sb, lstart, lend + 1);
                }
                else if (listToken.compareTo("CLIST") == 0)
                {
                    processClassList(sb, lstart, lend + 1, model, version);
                }
                else if (listToken.compareTo("MLIST") == 0)
                {
                    processMethodList(sb, lstart, lend + 1, thisClass);
                }
                else if (listToken.compareTo("FLIST") == 0)
                {
                    // Pass the FieldMap from either a class or a method.
                    // If this is called from a class-level template, we assume that the
                    // class field list is required. In this case, method will be null.
                    processFieldList(sb, lstart, lend + 1,
                                     (method == null ? thisClass.getFieldMap() : method.getFieldMap()),
                                     version);
                }
                else if (listToken.compareTo("TLIST") == 0)
                {
                    processConstantList(sb, lstart, lend + 1, _constantSet);
                }
                else
                {
                    throw new AmqpTemplateException("Unknown list token \"%{" + listToken +
                                                    "}\" found in template at index " + lstart + ".");
                }
            }
            lstart = sb.indexOf("%{", lstart + 1);
        }
    }

    protected void processAllTokens(StringBuffer sb, AmqpClass thisClass, AmqpMethod method, AmqpField field,
                                    AmqpVersion version)
    {
        int lstart = sb.indexOf("${");
        while (lstart != -1)
        {
            int lend = sb.indexOf("}", lstart + 2);
            if (lend > 0)
            {
                String token = sb.substring(lstart, lend + 1);
                replaceToken(sb, lstart, token, processToken(token, thisClass, method, field, version));
            }
            lstart = sb.indexOf("${", lstart);
        }
    }

    protected static void writeTargetFile(StringBuffer sb, File f)
    {
        try
        {
            f.getParentFile().mkdirs();
            FileWriter fw = new FileWriter(f);
            fw.write(sb.toString().toCharArray());
            fw.flush();
            fw.close();
        }
        catch (IOException e)
        {
            throw new AmqpTemplateException(e.getMessage());
        }
    }


    protected static String getTemplateFileName(StringBuffer sb)
    {
        if (sb.charAt(0) != '&')
        {
            throw new AmqpTemplateException("No filename marker &{filename} found at start of template.");
        }
        int cr = sb.indexOf(Utils.LINE_SEPARATOR);
        if (cr < 0)
        {
            throw new AmqpTemplateException("Bad template structure - unable to find first line.");
        }
        String fileName = sb.substring(2, cr - 1);
        sb.delete(0, cr + 1);
        return fileName;
    }

    protected static void replaceToken(StringBuffer sb, String token, String replacement)
    {
        replaceToken(sb, 0, token, replacement);
    }

    protected static void replaceToken(StringBuffer sb, int index, String token, String replacement)
    {
        if (replacement != null)
        {
            int start = sb.indexOf(token, index);
            if (start != -1)
            {
                int len = token.length();
                // Find first letter in token and determine if it is capitalized
                char firstTokenLetter = getFirstLetter(token);
                if (firstTokenLetter != 0 && Character.isUpperCase(firstTokenLetter))
                {
                    sb.replace(start, start + len, Utils.firstUpper(replacement));
                }
                else
                {
                    sb.replace(start, start + len, replacement);
                }
            }
        }
    }

    private static char getFirstLetter(String str)
    {
        int len = str.length();
        int index = 0;
        char tokChar = str.charAt(index);
        while (!Character.isLetter(tokChar) && index < len - 1)
        {
            tokChar = str.charAt(++index);
        }
        if (Character.isLetter(tokChar))
        {
            return tokChar;
        }
        return 0;
    }

    private static String loadTemplate(File f)
    {
        try
        {
            StringBuffer sb = new StringBuffer();
            FileReader fr = new FileReader(f);
            LineNumberReader lnr = new LineNumberReader(fr);
            String line = lnr.readLine();
            while (line != null)
            {

                sb.append(line);
                sb.append(Utils.LINE_SEPARATOR);

                line = lnr.readLine();
            }
            lnr.close();
            fr.close();
            return sb.toString();
        }
        catch (FileNotFoundException e)
        {
            throw new AmqpTemplateException("File not found: " + e.getMessage());
        }
        catch (IOException e)
        {
            throw new AmqpTemplateException("IOException: " + e.getMessage());
        }
    }

    public String getDomainType(String domainName, AmqpVersion version)
    {
        if (version == null)
        {
            version = _versionSet.first();
        }
        return getDomainMap().getDomainType(domainName, version);
    }


    public void addFromNode(Node amqpNode, AmqpVersion version)
    {
        // 1c. Extract domains
        getConstantSet().addFromNode(amqpNode, 0, version);
        _versionToConstantSetMap.get(version).addFromNode(amqpNode, 0, version);

        // 1d. Extract domains
        getDomainMap().addFromNode(amqpNode, 0, version);
        _versionToDomainMapMap.get(version).addFromNode(amqpNode, 0, version);

        // 1e. Extract class/method/field heirarchy
        getModel().addFromNode(amqpNode, 0, version);
        _versionToModelMap.get(version).addFromNode(amqpNode, 0, version);
    }


    public String getOutputDirectory()
    {
        return _outputDirectory;
    }

    public String prepareConstantName(String constantName)
    {
        return prepareDomainName(constantName);
    }


    public boolean isFixedSizeType(String type)
    {
        return FIXED_SIZE_TYPES.containsKey(type);
    }


    public int getTypeSize(String type)
    {
        return FIXED_SIZE_TYPES.get(type);
    }



    // Model-level template processing
    abstract protected void processModelTemplate(NamedTemplate template);

    // Class-level template processing
    abstract protected void processClassTemplate(NamedTemplate template, AmqpClass thisClass);

    // Method-level template processing
    abstract protected void processMethodTemplate(NamedTemplate template, AmqpClass thisClass,
                                                  AmqpMethod method);

    // Field-level template processing
    abstract protected void processTemplate(NamedTemplate template, AmqpClass thisClass,
                                            AmqpMethod method, AmqpField field, AmqpVersion version);

    abstract protected String prepareFilename(String filenameTemplate, AmqpClass thisClass, AmqpMethod method,
                                              AmqpField field, AmqpVersion version);

    abstract protected String processToken(String token, AmqpClass thisClass, AmqpMethod method,
                                           AmqpField field, AmqpVersion version);

    abstract protected void processClassList(StringBuffer sb, int listMarkerStartIndex, int listMarkerEndIndex,
                                             AmqpModel model, AmqpVersion version);

    abstract protected void processMethodList(StringBuffer sb, int listMarkerStartIndex, int listMarkerEndIndex,
                                              AmqpClass thisClass);


    abstract protected void processFieldList(StringBuffer sb, int listMarkerStartIndex, int listMarkerEndIndex,
                                             AmqpFieldMap fieldMap, AmqpVersion version);

    abstract protected void processConstantList(StringBuffer sb, int listMarkerStartIndex, int listMarkerEndIndex,
                                                AmqpConstantSet constantSet);


}
