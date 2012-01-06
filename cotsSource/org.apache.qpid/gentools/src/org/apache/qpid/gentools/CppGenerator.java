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

import java.io.File;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.TreeMap;

public class CppGenerator extends Generator
{
    protected static final String versionNamespaceStartToken = "${version_namespace_start}";
    protected static final String versionNamespaceEndToken = "${version_namespace_end}";

    // TODO: Move this to parent class
    protected static final int FIELD_NAME = 0;
    protected static final int FIELD_CODE_TYPE = 1;

    /**
     * A complete list of C++ reserved words. The names of varous XML elements within the AMQP
     * specification file are used for C++ identifier names in the generated code. Each proposed
     * name is checked against this list and is modified (by adding an '_' to the end of the
     * name - see function parseForReservedWords()) if found to be present.
     */
    protected static final String[] cppReservedWords = {"and", "and_eq", "asm", "auto", "bitand",
                                                        "bitor", "bool", "break", "case", "catch", "char", "class", "compl", "const", "const_cast",
                                                        "continue", "default", "delete", "do", "DomainInfo", "double", "dynamic_cast", "else",
                                                        "enum", "explicit", "extern", "false", "float", "for", "friend", "goto", "if", "inline",
                                                        "int", "long", "mutable", "namespace", "new", "not", "not_eq", "operator", "or", "or_eq",
                                                        "private", "protected", "public", "register", "reinterpret_cast", "return", "short",
                                                        "signed", "sizeof", "static", "static_cast", "struct", "switch", "template", "this",
                                                        "throw", "true", "try", "typedef", "typeid", "typename", "union", "unsigned", "using",
                                                        "virtual", "void", "volatile", "wchar_t", "while", "xor", "xor_eq"};

    /**
     * Although not reserved words, the following list of variable names that may cause compile
     * problems within a C++ environment because they clash with common #includes. The names of
     * varous XML elements within the AMQP specification file are used for C++ identifier names
     * in the generated code. Each proposed name is checked against this list and is modified
     * (by adding an '_' to the end of the name - see function parseForReservedWords()) if found
     * to be present. This list is best added to on an as-needed basis.
     */
    protected static final String[] cppCommonDefines = {"string"};

    // TODO: Move this to the Generator superclass?
    protected boolean quietFlag; // Supress warning messages to the console

    private class DomainInfo
    {
        public String type;
        public String size;
        public String encodeExpression;
        public String decodeExpression;

        public DomainInfo(String domain, String size, String encodeExpression,
                          String decodeExpression)
        {
            this.type = domain;
            this.size = size;
            this.encodeExpression = encodeExpression;
            this.decodeExpression = decodeExpression;
        }
    }

    private static TreeMap<String, DomainInfo> typeMap = new TreeMap<String, DomainInfo>();

    public CppGenerator()
    {
        super();
        quietFlag = true;
        // Load C++ type and size maps.
        // Adjust or add to these lists as new types are added/defined.
        // The char '#' will be replaced by the field variable name (any type).
        // The char '~' will be replaced by the compacted bit array size (type bit only).
        typeMap.put("bit", new DomainInfo(
                "bool",                    // type
                "~",                     // size
                "",                        // encodeExpression
                ""));                    // decodeExpression
        typeMap.put("content", new DomainInfo(
                "Content",                // type
                "#.size()",             // size
                "buffer.putContent(#)",    // encodeExpression
                "buffer.getContent(#)")); // decodeExpression
        typeMap.put("long", new DomainInfo(
                "u_int32_t",            // type
                "4",                     // size
                "buffer.putLong(#)",    // encodeExpression
                "# = buffer.getLong()"));    // decodeExpression
        typeMap.put("longlong", new DomainInfo(
                "u_int64_t",            // type
                "8",                     // size
                "buffer.putLongLong(#)", // encodeExpression
                "# = buffer.getLongLong()")); // decodeExpression
        typeMap.put("longstr", new DomainInfo(
                "string",                // type
                "4 + #.length()",         // size
                "buffer.putLongString(#)", // encodeExpression
                "buffer.getLongString(#)")); // decodeExpression
        typeMap.put("octet", new DomainInfo(
                "u_int8_t",                // type
                "1",                     // size
                "buffer.putOctet(#)",    // encodeExpression
                "# = buffer.getOctet()"));    // decodeExpression
        typeMap.put("short", new DomainInfo(
                "u_int16_t",            // type
                "2",                    // size
                "buffer.putShort(#)",    // encodeExpression
                "# = buffer.getShort()"));    // decodeExpression
        typeMap.put("shortstr", new DomainInfo(
                "string",                // type
                "1 + #.length()",        // size
                "buffer.putShortString(#)", // encodeExpression
                "buffer.getShortString(#)")); // decodeExpression
        typeMap.put("table", new DomainInfo(
                "FieldTable",            // type
                "#.size()",             // size
                "buffer.putFieldTable(#)", // encodeExpression
                "buffer.getFieldTable(#)")); // decodeExpression
        typeMap.put("timestamp", new DomainInfo(
                "u_int64_t",            // type
                "8",                     // size
                "buffer.putLongLong(#)", // encodeExpression
                "buffer.getLongLong(#)")); // decodeExpression
    }


    public boolean isQuietFlag()
    {
        return quietFlag;
    }

    public void setQuietFlag(boolean quietFlag)
    {
        this.quietFlag = quietFlag;
    }

    // === Start of methods for Interface LanguageConverter ===

    public String prepareClassName(String className)
    {
        return camelCaseName(className, true);
    }

    public String prepareMethodName(String methodName)
    {
        return camelCaseName(methodName, false);
    }

    public String prepareDomainName(String domainName)
    {
        return camelCaseName(domainName, false);
    }


    public String getGeneratedType(String domainName, AmqpVersion version)
            throws AmqpTypeMappingException
    {
        String domainType = getDomainType(domainName, version);
        if (domainType == null)
        {
            throw new AmqpTypeMappingException("Domain type \"" + domainName +
                                               "\" not found in C++ typemap.");
        }
        DomainInfo info = typeMap.get(domainType);
        if (info == null)
        {
            throw new AmqpTypeMappingException("Unknown domain: \"" + domainType + "\"");
        }
        return info.type;
    }

    // === Abstract methods from class Generator - C++-specific implementation ===

    @Override
    protected String prepareFilename(String filenameTemplate, AmqpClass thisClass, AmqpMethod method,
                                     AmqpField field, AmqpVersion version)
    {
        StringBuffer sb = new StringBuffer(filenameTemplate);
        if (thisClass != null)
        {
            replaceToken(sb, "${CLASS}", thisClass.getName());
        }
        if (method != null)
        {
            replaceToken(sb, "${METHOD}", method.getName());
        }
        if (field != null)
        {
            replaceToken(sb, "${FIELD}", field.getName());
        }
        return sb.toString();
    }

    @Override
    protected void processModelTemplate(NamedTemplate template)
    {
        processTemplate(template, null, null, null, null);
    }

    @Override
    protected void processClassTemplate(NamedTemplate template, AmqpClass thisClass)
    {
        processTemplate(template, thisClass, null, null, null);
    }

    @Override
    protected void processMethodTemplate(NamedTemplate template, AmqpClass thisClass,
                                         AmqpMethod method)
    {
        StringBuffer sb = new StringBuffer(template.getTemplate());
        String filename = prepareFilename(getTemplateFileName(sb), thisClass, method, null, null);
        boolean templateProcessedFlag = false;

        // If method is not version consistent, create a namespace for each version
        // i.e. copy the bit between the versionNamespaceStartToken and versionNamespaceEndToken
        // once for each namespace.
        if (method != null)
        {
            if (!method.isVersionConsistent(getVersionSet()))
            {
                int namespaceStartIndex = sb.indexOf(versionNamespaceStartToken);
                int namespaceEndIndex = sb.indexOf(versionNamespaceEndToken) +
                                        versionNamespaceEndToken.length();
                if (namespaceStartIndex >= 0 && namespaceEndIndex >= 0 &&
                    namespaceStartIndex <= namespaceEndIndex)
                {
                    String namespaceSpan = sb.substring(namespaceStartIndex, namespaceEndIndex) + CR;
                    sb.delete(namespaceStartIndex, namespaceEndIndex);
                    for (AmqpVersion v : method.getVersionSet())
                    {
                        StringBuffer nssb = new StringBuffer(namespaceSpan);
                        processTemplate(nssb, thisClass, method, null, template.getName(), v);
                        sb.insert(namespaceStartIndex, nssb);
                    }
                    // Process all tokens *not* within the namespace span prior to inserting namespaces
                    processTemplate(sb, thisClass, method, null, template.getName(), null);
                }
                templateProcessedFlag = true;
            }
        }
        // Remove any remaining namespace tags
        int nsTokenIndex = sb.indexOf(versionNamespaceStartToken);
        while (nsTokenIndex > 0)
        {
            sb.delete(nsTokenIndex, nsTokenIndex + versionNamespaceStartToken.length());
            nsTokenIndex = sb.indexOf(versionNamespaceStartToken);
        }
        nsTokenIndex = sb.indexOf(versionNamespaceEndToken);
        while (nsTokenIndex > 0)
        {
            sb.delete(nsTokenIndex, nsTokenIndex + versionNamespaceEndToken.length());
            nsTokenIndex = sb.indexOf(versionNamespaceEndToken);
        }

        if (!templateProcessedFlag)
        {
            processTemplate(sb, thisClass, method, null, template.getName(), null);
        }
        writeTargetFile(sb, new File(getOutputDirectory() + Utils.FILE_SEPARATOR + filename));
        generatedFileCounter++;
    }

    @Override
    protected void processTemplate(NamedTemplate template, AmqpClass thisClass, AmqpMethod method,
                                   AmqpField field, AmqpVersion version)
    {
        StringBuffer sb = new StringBuffer(template.getTemplate());
        String filename = prepareFilename(getTemplateFileName(sb), thisClass, method, field, version);
        processTemplate(sb, thisClass, method, field, template.getName(), null);
        writeTargetFile(sb, new File(getOutputDirectory() + Utils.FILE_SEPARATOR + filename));
        generatedFileCounter++;
    }

    protected void processTemplate(StringBuffer sb, AmqpClass thisClass, AmqpMethod method,
                                   AmqpField field, String templateFileName, AmqpVersion version)
    {
        try
        {
            processAllLists(sb, thisClass, method, version);
        }
        catch (AmqpTemplateException e)
        {
            System.out.println("ERROR: " + templateFileName + ": " + e.getMessage());
        }
        try
        {
            processAllTokens(sb, thisClass, method, field, version);
        }
        catch (AmqpTemplateException e)
        {
            System.out.println("ERROR: " + templateFileName + ": " + e.getMessage());
        }
    }

    @Override
    protected String processToken(String token, AmqpClass thisClass, AmqpMethod method, AmqpField field,
                                  AmqpVersion version)
    {
        if (token.compareTo("${GENERATOR}") == 0)
        {
            return GENERATOR_INFO;
        }
        if (token.compareTo("${CLASS}") == 0 && thisClass != null)
        {
            return thisClass.getName();
        }
        if (token.compareTo("${CLASS_ID_INIT}") == 0 && thisClass != null)
        {
            if (version == null)
            {
                return String.valueOf(thisClass.getIndexMap().firstKey());
            }
            return getIndex(thisClass.getIndexMap(), version);
        }
        if (token.compareTo("${METHOD}") == 0 && method != null)
        {
            return method.getName();
        }
        if (token.compareTo("${METHOD_ID_INIT}") == 0 && method != null)
        {
            if (version == null)
            {
                return String.valueOf(method.getIndexMap().firstKey());
            }
            return getIndex(method.getIndexMap(), version);
        }
        if (token.compareTo("${FIELD}") == 0 && field != null)
        {
            return field.getName();
        }
        if (token.compareTo(versionNamespaceStartToken) == 0 && version != null)
        {
            return "namespace " + version.namespace() + CR + "{";
        }
        if (token.compareTo(versionNamespaceEndToken) == 0 && version != null)
        {
            return "} // namespace " + version.namespace();
        }
        if (token.compareTo("${mb_constructor_with_initializers}") == 0)
        {
            return generateConstructor(thisClass, method, version, 4, 4);
        }
        if (token.compareTo("${mb_server_operation_invoke}") == 0)
        {
            return generateServerOperationsInvoke(thisClass, method, version, 4, 4);
        }
        if (token.compareTo("${mb_buffer_param}") == 0)
        {
            return method.getFieldMap().size() > 0 ? " buffer" : "";
        }
        if (token.compareTo("${hv_latest_major}") == 0)
        {
            return String.valueOf(getVersionSet().last().getMajor());
        }
        if (token.compareTo("${hv_latest_minor}") == 0)
        {
            return String.valueOf(getVersionSet().last().getMinor());
        }

        throw new AmqpTemplateException("Template token " + token + " unknown.");
    }

    @Override
    protected void processClassList(StringBuffer sb, int listMarkerStartIndex, int listMarkerEndIndex,
                                    AmqpModel model, AmqpVersion version)
    {
        String codeSnippet;
        int lend = sb.indexOf(CR, listMarkerStartIndex) + 1; // Include cr at end of line
        String tline = sb.substring(listMarkerEndIndex, lend); // Line excluding line marker, including cr
        int tokxStart = tline.indexOf('$');
        String token = tline.substring(tokxStart).trim();
        sb.delete(listMarkerStartIndex, lend);

        // ClientOperations.h
        if (token.compareTo("${coh_method_handler_get_method}") == 0)
        {
            codeSnippet = generateOpsMethodHandlerGetMethods(model, false, 4);
        }
        else if (token.compareTo("${coh_inner_class}") == 0)
        {
            codeSnippet = generateOpsInnerClasses(model, false, 4, 4);
        }

        // ServerOperations.h
        else if (token.compareTo("${soh_method_handler_get_method}") == 0)
        {
            codeSnippet = generateOpsMethodHandlerGetMethods(model, true, 4);
        }
        else if (token.compareTo("${soh_inner_class}") == 0)
        {
            codeSnippet = generateOpsInnerClasses(model, true, 4, 4);
        }

        // ClientProxy.h/cpp
        else if (token.compareTo("${cph_inner_class_instance}") == 0)
        {
            codeSnippet = generateProxyInnerClassInstances(model, false, 4);
        }
        else if (token.compareTo("${cph_inner_class_get_method}") == 0)
        {
            codeSnippet = generateProxyInnerClassGetMethodDecls(model, false, 4);
        }
        else if (token.compareTo("${cph_inner_class_defn}") == 0)
        {
            codeSnippet = generateProxyInnerClassDefinitions(model, false, 4, 4);
        }
        else if (token.compareTo("${cpc_constructor_initializer}") == 0)
        {
            codeSnippet = generateProxyConstructorInitializers(model, false, 4);
        }
        else if (token.compareTo("${cpc_inner_class_get_method}") == 0)
        {
            codeSnippet = generateProxyInnerClassGetMethodImpls(model, false, 0, 4);
        }
        else if (token.compareTo("${cpc_inner_class_impl}") == 0)
        {
            codeSnippet = generateProxyInnerClassImpl(model, false, 0, 4);
        }
        else if (token.compareTo("${cph_handler_pointer_defn}") == 0)
        {
            codeSnippet = generateHandlerPointerDefinitions(model, false, 4);
        }
        else if (token.compareTo("${cph_handler_pointer_get_method}") == 0)
        {
            codeSnippet = generateHandlerPointerGetMethods(model, false, 4);
        }

        // SerrverProxy.h/cpp
        else if (token.compareTo("${sph_inner_class_instance}") == 0)
        {
            codeSnippet = generateProxyInnerClassInstances(model, true, 4);
        }
        else if (token.compareTo("${sph_inner_class_get_method}") == 0)
        {
            codeSnippet = generateProxyInnerClassGetMethodDecls(model, true, 4);
        }
        else if (token.compareTo("${sph_inner_class_defn}") == 0)
        {
            codeSnippet = generateProxyInnerClassDefinitions(model, true, 4, 4);
        }
        else if (token.compareTo("${spc_constructor_initializer}") == 0)
        {
            codeSnippet = generateProxyConstructorInitializers(model, true, 4);
        }
        else if (token.compareTo("${spc_inner_class_get_method}") == 0)
        {
            codeSnippet = generateProxyInnerClassGetMethodImpls(model, true, 0, 4);
        }
        else if (token.compareTo("${spc_inner_class_impl}") == 0)
        {
            codeSnippet = generateProxyInnerClassImpl(model, true, 0, 4);
        }
        else if (token.compareTo("${sph_handler_pointer_defn}") == 0)
        {
            codeSnippet = generateHandlerPointerDefinitions(model, true, 4);
        }
        else if (token.compareTo("${sph_handler_pointer_get_method}") == 0)
        {
            codeSnippet = generateHandlerPointerGetMethods(model, true, 4);
        }

        // amqp_methods.h/cpp
        else if (token.compareTo("${mh_method_body_class_indlude}") == 0)
        {
            codeSnippet = generateMethodBodyIncludeList(model, 0);
        }
        else if (token.compareTo("${mh_method_body_class_instance}") == 0)
        {
            codeSnippet = generateMethodBodyInstances(model, 0);
        }
        else if (token.compareTo("${mc_create_method_body_map_entry}") == 0)
        {
            codeSnippet = generateMethodBodyMapEntry(model, 4);
        }

        else // Oops!
        {
            throw new AmqpTemplateException("Template token \"" + token + "\" unknown.");
        }
        sb.insert(listMarkerStartIndex, codeSnippet);
    }

    @Override
    protected void processMethodList(StringBuffer sb, int listMarkerStartIndex, int listMarkerEndIndex,
                                     AmqpClass thisClass)
    {
        String codeSnippet;
        int lend = sb.indexOf(CR, listMarkerStartIndex) + 1; // Include cr at end of line
        String tline = sb.substring(listMarkerEndIndex, lend); // Line excluding line marker, including cr
        int tokxStart = tline.indexOf('$');
        String token = tline.substring(tokxStart).trim();
        sb.delete(listMarkerStartIndex, lend);

        if (token.compareTo("${cpc_method_body_include}") == 0)
        {
            codeSnippet = generateMethodBodyIncludes(thisClass, 0);
        }
        else if (token.compareTo("${spc_method_body_include}") == 0)
        {
            codeSnippet = generateMethodBodyIncludes(thisClass, 0);
        }
        else if (token.compareTo("${mc_method_body_include}") == 0)
        {
            codeSnippet = generateMethodBodyIncludes(thisClass, 0);
        }

        else // Oops!
        {
            throw new AmqpTemplateException("Template token " + token + " unknown.");
        }
        sb.insert(listMarkerStartIndex, codeSnippet);
    }

    @Override
    protected void processFieldList(StringBuffer sb, int listMarkerStartIndex, int listMarkerEndIndex,
                                    AmqpFieldMap fieldMap, AmqpVersion version)
    {
        String codeSnippet;
        int lend = sb.indexOf(CR, listMarkerStartIndex) + 1; // Include cr at end of line
        String tline = sb.substring(listMarkerEndIndex, lend); // Line excluding line marker, including cr
        int tokxStart = tline.indexOf('$');
        String token = tline.substring(tokxStart).trim();
        sb.delete(listMarkerStartIndex, lend);

        if (token.compareTo("${mb_field_declaration}") == 0)
        {
            codeSnippet = generateFieldDeclarations(fieldMap, version, 4);
        }
        else if (token.compareTo("${mb_field_get_method}") == 0)
        {
            codeSnippet = generateFieldGetMethods(fieldMap, version, 4);
        }
        else if (token.compareTo("${mb_field_print}") == 0)
        {
            codeSnippet = generatePrintMethodContents(fieldMap, version, 8);
        }
        else if (token.compareTo("${mb_body_size}") == 0)
        {
            codeSnippet = generateBodySizeMethodContents(fieldMap, version, 8);
        }
        else if (token.compareTo("${mb_encode}") == 0)
        {
            codeSnippet = generateEncodeMethodContents(fieldMap, version, 8);
        }
        else if (token.compareTo("${mb_decode}") == 0)
        {
            codeSnippet = generateDecodeMethodContents(fieldMap, version, 8);
        }

        else // Oops!
        {
            throw new AmqpTemplateException("Template token " + token + " unknown.");
        }
        sb.insert(listMarkerStartIndex, codeSnippet);
    }

    @Override
    protected void processConstantList(StringBuffer sb, int listMarkerStartIndex, int listMarkerEndIndex,
                                       AmqpConstantSet constantSet)
    {
        String codeSnippet;
        int lend = sb.indexOf(CR, listMarkerStartIndex) + 1; // Include cr at end of line
        String tline = sb.substring(listMarkerEndIndex, lend); // Line excluding line marker, including cr
        int tokxStart = tline.indexOf('$');
        String token = tline.substring(tokxStart).trim();
        sb.delete(listMarkerStartIndex, lend);

        if (token.compareTo("${ch_get_value_method}") == 0)
        {
            codeSnippet = generateConstantGetMethods(constantSet, 4, 4);
        }

        else // Oops!
        {
            throw new AmqpTemplateException("Template token " + token + " unknown.");
        }
        sb.insert(listMarkerStartIndex, codeSnippet);
    }

    // === Protected and private helper functions unique to C++ implementation ===

    // Methods for generation of code snippets for AMQP_Constants.h file

    protected String generateConstantGetMethods(AmqpConstantSet constantSet,
                                                int indentSize, int tabSize)
    {
        String indent = Utils.createSpaces(indentSize);
        StringBuffer sb = new StringBuffer();
        for (AmqpConstant thisConstant : constantSet.getContstants())
        {
            if (thisConstant.isVersionConsistent(getVersionSet()))
            {
                // return a constant
                String value = thisConstant.firstKey();
                sb.append(indent + "static const char* " + thisConstant.getName() + "() { return \"" +
                          thisConstant.firstKey() + "\"; }" + CR);
                if (Utils.containsOnlyDigits(value))
                {
                    sb.append(indent + "static int " + thisConstant.getName() + "AsInt() { return " +
                              thisConstant.firstKey() + "; }" + CR);
                }
                if (Utils.containsOnlyDigitsAndDecimal(value))
                {
                    sb.append(indent + "static double " + thisConstant.getName() + "AsDouble() { return (double)" +
                              thisConstant.firstKey() + "; }" + CR);
                }
                sb.append(CR);
            }
            else
            {
                // Return version-specific constant
                sb.append(generateVersionDependentGet(thisConstant, "const char*", "", "\"", "\"", indentSize, tabSize));
                sb.append(generateVersionDependentGet(thisConstant, "int", "AsInt", "", "", indentSize, tabSize));
                sb.append(generateVersionDependentGet(thisConstant, "double", "AsDouble", "(double)", "", indentSize, tabSize));
                sb.append(CR);
            }
        }
        return sb.toString();
    }

    protected String generateVersionDependentGet(AmqpConstant constant, String methodReturnType,
                                                 String methodNameSuffix, String returnPrefix, String returnPostfix, int indentSize, int tabSize)
    {
        String indent = Utils.createSpaces(indentSize);
        String tab = Utils.createSpaces(tabSize);
        StringBuffer sb = new StringBuffer();
        sb.append(indent + methodReturnType + " " + constant.getName() + methodNameSuffix +
                  "() const" + CR);
        sb.append(indent + "{" + CR);
        boolean first = true;
        for (String thisValue : constant.keySet())
        {
            AmqpVersionSet versionSet = constant.get(thisValue);
            sb.append(indent + tab + (first ? "" : "else ") + "if (" + generateVersionCheck(versionSet) +
                      ")" + CR);
            sb.append(indent + tab + "{" + CR);
            if (methodReturnType.compareTo("int") == 0 && !Utils.containsOnlyDigits(thisValue))
            {
                sb.append(generateConstantDeclarationException(constant.getName(), methodReturnType,
                                                               indentSize + (2 * tabSize), tabSize));
            }
            else if (methodReturnType.compareTo("double") == 0 && !Utils.containsOnlyDigitsAndDecimal(thisValue))
            {
                sb.append(generateConstantDeclarationException(constant.getName(), methodReturnType,
                                                               indentSize + (2 * tabSize), tabSize));
            }
            else
            {
                sb.append(indent + tab + tab + "return " + returnPrefix + thisValue + returnPostfix + ";" + CR);
            }
            sb.append(indent + tab + "}" + CR);
            first = false;
        }
        sb.append(indent + tab + "else" + CR);
        sb.append(indent + tab + "{" + CR);
        sb.append(indent + tab + tab + "std::stringstream ss;" + CR);
        sb.append(indent + tab + tab + "ss << \"Constant \\\"" + constant.getName() +
                  "\\\" is undefined for AMQP version \" <<" + CR);
        sb.append(indent + tab + tab + tab + "version.toString() << \".\";" + CR);
        sb.append(indent + tab + tab + "throw ProtocolVersionException(ss.str());" + CR);
        sb.append(indent + tab + "}" + CR);
        sb.append(indent + "}" + CR);
        return sb.toString();
    }

    protected String generateConstantDeclarationException(String name, String methodReturnType,
                                                          int indentSize, int tabSize)
    {
        String indent = Utils.createSpaces(indentSize);
        String tab = Utils.createSpaces(tabSize);
        StringBuffer sb = new StringBuffer();
        sb.append(indent + "std::stringstream ss;" + CR);
        sb.append(indent + "ss << \"Constant \\\"" + name + "\\\" cannot be converted to type " +
                  methodReturnType + " for AMQP version \" <<" + CR);
        sb.append(indent + tab + "version.toString() << \".\";" + CR);
        sb.append(indent + "throw ProtocolVersionException(ss.str());" + CR);
        return sb.toString();
    }

    // Methods used for generation of code snippets for Server/ClientOperations class generation

    protected String generateOpsMethodHandlerGetMethods(AmqpModel model, boolean serverFlag, int indentSize)
    {
        String indent = Utils.createSpaces(indentSize);
        StringBuffer sb = new StringBuffer();
        for (String thisClassName : model.getClassMap().keySet())
        {
            AmqpClass thisClass = model.getClassMap().get(thisClassName);
            // Only generate for this class if there is at least one method of the
            // required chassis (server/client flag).
            boolean chassisFoundFlag = false;
            for (String thisMethodName : thisClass.getMethodMap().keySet())
            {
                AmqpMethod method = thisClass.getMethodMap().get(thisMethodName);
                boolean clientChassisFlag = method.getClientMethodFlagMap().isSet();
                boolean serverChassisFlag = method.getServerMethodFlagMap().isSet();
                if ((serverFlag && serverChassisFlag) || (!serverFlag && clientChassisFlag))
                {
                    chassisFoundFlag = true;
                }
            }
            if (chassisFoundFlag)
            {
                sb.append(indent + "virtual AMQP_" + (serverFlag ? "Server" : "Client") + "Operations::" +
                          thisClass.getName() + "Handler* get" + thisClass.getName() + "Handler() = 0;" + CR);
            }
        }
        return sb.toString();
    }

    protected String generateOpsInnerClasses(AmqpModel model, boolean serverFlag, int indentSize, int tabSize)
    {

        String proxyClassName = "AMQP_" + (serverFlag ? "Server" : "Client") + "Proxy";
        String indent = Utils.createSpaces(indentSize);
        String tab = Utils.createSpaces(tabSize);
        StringBuffer sb = new StringBuffer();
        boolean first = true;
        for (String thisClassName : model.getClassMap().keySet())
        {
            AmqpClass thisClass = model.getClassMap().get(thisClassName);
            String handlerClassName = thisClass.getName() + "Handler";
            if (!first)
            {
                sb.append(CR);
            }
            sb.append(indent + "// ==================== class " + handlerClassName +
                      " ====================" + CR);
            sb.append(indent + "class " + handlerClassName);
            if (thisClass.getVersionSet().size() != getVersionSet().size())
            {
                sb.append(" // AMQP Version(s) " + thisClass.getVersionSet() + CR);
            }
            else
            {
                sb.append(CR);
            }
            sb.append(indent + "{" + CR);
            sb.append(indent + "private:" + CR);
            sb.append(indent + tab + proxyClassName + "* parent;" + CR);
            sb.append(CR);
            sb.append(indent + tab + "// Constructors and destructors" + CR);
            sb.append(CR);
            sb.append(indent + "protected:" + CR);
            sb.append(indent + tab + handlerClassName + "() {}" + CR);
            sb.append(indent + "public:" + CR);
            sb.append(indent + tab + handlerClassName +
                      "(" + proxyClassName + "* _parent) {parent = _parent;}" + CR);
            sb.append(indent + tab + "virtual ~" + handlerClassName + "() {}" + CR);
            sb.append(CR);
            sb.append(indent + tab + "// Protocol methods" + CR);
            sb.append(CR);
            sb.append(generateInnerClassMethods(thisClass, serverFlag, true, indentSize + tabSize, tabSize));
            sb.append(indent + "}; // class " + handlerClassName + CR);
            first = false;
        }
        return sb.toString();
    }

    protected String generateInnerClassMethods(AmqpClass thisClass, boolean serverFlag,
                                               boolean abstractMethodFlag, int indentSize, int tabSize)
    {
        String indent = Utils.createSpaces(indentSize);
        StringBuffer sb = new StringBuffer();
        String outerClassName = "AMQP_" + (serverFlag ? "Server" : "Client") + (abstractMethodFlag ? "Operations"
                                                                                : "Proxy");
        boolean first = true;
        for (String thisMethodName : thisClass.getMethodMap().keySet())
        {
            AmqpMethod method = thisClass.getMethodMap().get(thisMethodName);
            boolean clientChassisFlag = method.getClientMethodFlagMap().isSet();
            boolean serverChassisFlag = method.getServerMethodFlagMap().isSet();
            if ((serverFlag && serverChassisFlag) || (!serverFlag && clientChassisFlag))
            {
                String methodName = parseForReservedWords(method.getName(), outerClassName + "." + thisClass.getName());
                AmqpOverloadedParameterMap overloadededParameterMap =
                        method.getOverloadedParameterLists(thisClass.getVersionSet(), this);
                for (AmqpOrdinalFieldMap thisFieldMap : overloadededParameterMap.keySet())
                {
                    AmqpVersionSet versionSet = overloadededParameterMap.get(thisFieldMap);
                    if (!first)
                    {
                        sb.append(CR);
                    }
                    sb.append(indent + "virtual void " + methodName + "( u_int16_t channel");
                    sb.append(generateMethodParameterList(thisFieldMap, indentSize + (5 * tabSize), true, true, true));
                    sb.append(" )");
                    if (abstractMethodFlag)
                    {
                        sb.append(" = 0");
                    }
                    sb.append(";");
                    if (versionSet.size() != getVersionSet().size())
                    {
                        sb.append(" // AMQP Version(s) " + versionSet);
                    }
                    sb.append(CR);
                    first = false;
                }
            }
        }
        return sb.toString();
    }

    // Methods used for generation of code snippets for Server/ClientProxy class generation

    protected String generateHandlerPointerDefinitions(AmqpModel model, boolean serverFlag,
                                                       int indentSize)
    {
        String indent = Utils.createSpaces(indentSize);
        StringBuffer sb = new StringBuffer();
        String outerClassName = "AMQP_" + (serverFlag ? "Server" : "Client") + "Operations";
        for (String thisClassName : model.getClassMap().keySet())
        {
            AmqpClass thisClass = model.getClassMap().get(thisClassName);
            sb.append(indent + outerClassName + "::" + thisClass.getName() + "Handler* " +
                      thisClass.getName() + "HandlerPtr;" + CR);
        }
        return sb.toString();
    }

    protected String generateHandlerPointerGetMethods(AmqpModel model, boolean serverFlag,
                                                      int indentSize)
    {
        String indent = Utils.createSpaces(indentSize);
        StringBuffer sb = new StringBuffer();
        String outerClassName = "AMQP_" + (serverFlag ? "Server" : "Client") + "Operations";
        for (String thisClassName : model.getClassMap().keySet())
        {
            AmqpClass thisClass = model.getClassMap().get(thisClassName);
            sb.append(indent + "virtual inline " + outerClassName + "::" + thisClass.getName() + "Handler* get" +
                      thisClass.getName() + "Handler() { return &" + Utils.firstLower(thisClass.getName()) + ";}" + CR);
        }
        return sb.toString();
    }

    protected String generateProxyInnerClassInstances(AmqpModel model, boolean serverFlag,
                                                      int indentSize)
    {
        String indent = Utils.createSpaces(indentSize);
        StringBuffer sb = new StringBuffer();
        String outerClassName = "AMQP_" + (serverFlag ? "Server" : "Client") + "Proxy";
        for (String thisClassName : model.getClassMap().keySet())
        {
            AmqpClass thisClass = model.getClassMap().get(thisClassName);
            String instanceName = parseForReservedWords(Utils.firstLower(thisClass.getName()), outerClassName);
            String className = parseForReservedWords(thisClass.getName(), null);
            sb.append(indent + className + " " + instanceName + ";");
            if (thisClass.getVersionSet().size() != getVersionSet().size())
            {
                sb.append(" // AMQP Version(s) " + thisClass.getVersionSet() + CR);
            }
            else
            {
                sb.append(CR);
            }
        }
        return sb.toString();
    }

    protected String generateProxyInnerClassGetMethodDecls(AmqpModel model, boolean serverFlag,
                                                           int indentSize)
    {
        String indent = Utils.createSpaces(indentSize);
        StringBuffer sb = new StringBuffer();
        String outerClassName = "AMQP_" + (serverFlag ? "Server" : "Client") + "Proxy";
        for (String thisClassName : model.getClassMap().keySet())
        {
            AmqpClass thisClass = model.getClassMap().get(thisClassName);
            String className = parseForReservedWords(thisClass.getName(), outerClassName);
            sb.append(indent + className + "& get" + className + "();");
            if (thisClass.getVersionSet().size() != getVersionSet().size())
            {
                sb.append(" // AMQP Version(s) " + thisClass.getVersionSet() + CR);
            }
            else
            {
                sb.append(CR);
            }
        }
        return sb.toString();
    }

    protected String generateProxyInnerClassDefinitions(AmqpModel model, boolean serverFlag,
                                                        int indentSize, int tabSize)
    {
        String proxyClassName = "AMQP_" + (serverFlag ? "Server" : "Client") + "Proxy";
        String indent = Utils.createSpaces(indentSize);
        String tab = Utils.createSpaces(tabSize);
        StringBuffer sb = new StringBuffer();
        boolean first = true;
        for (String thisClassName : model.getClassMap().keySet())
        {
            AmqpClass thisClass = model.getClassMap().get(thisClassName);
            String className = thisClass.getName();
            String superclassName = "AMQP_" + (serverFlag ? "Server" : "Client") + "Operations::" +
                                    thisClass.getName() + "Handler";
            if (!first)
            {
                sb.append(CR);
            }
            sb.append(indent + "// ==================== class " + className +
                      " ====================" + CR);
            sb.append(indent + "class " + className + " : virtual public " + superclassName);
            if (thisClass.getVersionSet().size() != getVersionSet().size())
            {
                sb.append(" // AMQP Version(s) " + thisClass.getVersionSet() + CR);
            }
            else
            {
                sb.append(CR);
            }
            sb.append(indent + "{" + CR);
            sb.append(indent + "private:" + CR);
            sb.append(indent + tab + "OutputHandler* out;" + CR);
            sb.append(indent + tab + proxyClassName + "* parent;" + CR);
            sb.append(CR);
            sb.append(indent + "public:" + CR);
            sb.append(indent + tab + "// Constructors and destructors" + CR);
            sb.append(CR);
            sb.append(indent + tab + className + "(OutputHandler* out, " + proxyClassName + "* _parent) : " + CR);
            sb.append(indent + tab + tab + "out(out) {parent = _parent;}" + CR);
            sb.append(indent + tab + "virtual ~" + className + "() {}" + CR);
            sb.append(CR);
            sb.append(indent + tab + "// Protocol methods" + CR);
            sb.append(CR);
            sb.append(generateInnerClassMethods(thisClass, serverFlag, false, indentSize + tabSize, tabSize));
            sb.append(indent + "}; // class " + className + CR);
            first = false;
        }
        return sb.toString();
    }

    protected String generateProxyConstructorInitializers(AmqpModel model, boolean serverFlag,
                                                          int indentSize)
    {
        String outerClassName = "AMQP_" + (serverFlag ? "Server" : "Client") + "Proxy";
        String superclassName = "AMQP_" + (serverFlag ? "Server" : "Client") + "Operations";
        String indent = Utils.createSpaces(indentSize);
        StringBuffer sb = new StringBuffer(indent + superclassName + "(major, minor)," + CR);
        sb.append(indent + "version(major, minor)," + CR);
        sb.append(indent + "out(out)");
        Iterator<String> cItr = model.getClassMap().keySet().iterator();
        while (cItr.hasNext())
        {
            AmqpClass thisClass = model.getClassMap().get(cItr.next());
            String instanceName = parseForReservedWords(Utils.firstLower(thisClass.getName()), outerClassName);
            sb.append("," + CR);
            sb.append(indent + instanceName + "(out, this)");
            if (!cItr.hasNext())
            {
                sb.append(CR);
            }
        }
        return sb.toString();
    }

    protected String generateProxyInnerClassGetMethodImpls(AmqpModel model, boolean serverFlag,
                                                           int indentSize, int tabSize)
    {
        String indent = Utils.createSpaces(indentSize);
        String tab = Utils.createSpaces(tabSize);
        StringBuffer sb = new StringBuffer();
        String outerClassName = "AMQP_" + (serverFlag ? "Server" : "Client") + "Proxy";
        Iterator<String> cItr = model.getClassMap().keySet().iterator();
        while (cItr.hasNext())
        {
            AmqpClass thisClass = model.getClassMap().get(cItr.next());
            String className = thisClass.getName();
            String instanceName = parseForReservedWords(Utils.firstLower(thisClass.getName()), outerClassName);
            sb.append(indent + outerClassName + "::" + className + "& " +
                      outerClassName + "::get" + className + "()" + CR);
            sb.append(indent + "{" + CR);
            if (thisClass.getVersionSet().size() != getVersionSet().size())
            {
                sb.append(indent + tab + "if (!" + generateVersionCheck(thisClass.getVersionSet()) + ")" + CR);
                sb.append(indent + tab + tab + "throw new ProtocolVersionException();" + CR);
            }
            sb.append(indent + tab + "return " + instanceName + ";" + CR);
            sb.append(indent + "}" + CR);
            if (cItr.hasNext())
            {
                sb.append(CR);
            }
        }
        return sb.toString();
    }

    protected String generateProxyInnerClassImpl(AmqpModel model, boolean serverFlag,
                                                 int indentSize, int tabSize)
    {
        String indent = Utils.createSpaces(indentSize);
        StringBuffer sb = new StringBuffer();
        boolean firstClassFlag = true;
        for (String thisClassName : model.getClassMap().keySet())
        {
            AmqpClass thisClass = model.getClassMap().get(thisClassName);
            String className = thisClass.getName();
            if (!firstClassFlag)
            {
                sb.append(CR);
            }
            sb.append(indent + "// ==================== class " + className +
                      " ====================" + CR);
            sb.append(generateInnerClassMethodImpls(thisClass, serverFlag, indentSize, tabSize));
            firstClassFlag = false;
        }
        return sb.toString();
    }

    protected String generateInnerClassMethodImpls(AmqpClass thisClass, boolean serverFlag,
                                                   int indentSize, int tabSize)
    {
        String indent = Utils.createSpaces(indentSize);
        StringBuffer sb = new StringBuffer();
        String outerclassName = "AMQP_" + (serverFlag ? "Server" : "Client") + "Proxy";
        boolean first = true;
        for (String thisMethodName : thisClass.getMethodMap().keySet())
        {
            AmqpMethod method = thisClass.getMethodMap().get(thisMethodName);
            String methodBodyClassName = thisClass.getName() + Utils.firstUpper(method.getName()) + "Body";
            boolean clientChassisFlag = method.getClientMethodFlagMap().isSet();
            boolean serverChassisFlag = method.getServerMethodFlagMap().isSet();
            boolean versionConsistentFlag = method.isVersionConsistent(getVersionSet());
            if ((serverFlag && serverChassisFlag) || (!serverFlag && clientChassisFlag))
            {
                String methodName = parseForReservedWords(method.getName(), outerclassName + "." + thisClass.getName());
                AmqpOverloadedParameterMap overloadededParameterMap =
                        method.getOverloadedParameterLists(thisClass.getVersionSet(), this);
                for (AmqpOrdinalFieldMap thisFieldMap : overloadededParameterMap.keySet())
                {
                    AmqpVersionSet versionSet = overloadededParameterMap.get(thisFieldMap);
                    if (!first)
                    {
                        sb.append(CR);
                    }
                    sb.append(indent + "void " + outerclassName + "::" + thisClass.getName() + "::" +
                              methodName + "( u_int16_t channel");
                    sb.append(generateMethodParameterList(thisFieldMap, indentSize + (5 * tabSize), true, true, true));
                    sb.append(" )");
                    if (versionSet.size() != getVersionSet().size())
                    {
                        sb.append(" // AMQP Version(s) " + versionSet);
                    }
                    sb.append(CR);
                    sb.append(indent + "{" + CR);
                    sb.append(generateMethodBodyCallContext(thisFieldMap, outerclassName, methodBodyClassName,
                                                            versionConsistentFlag, versionSet, indentSize + tabSize, tabSize));
                    sb.append(indent + "}" + CR);
                    sb.append(CR);
                    first = false;
                }
            }
        }
        return sb.toString();
    }

    protected String generateMethodBodyCallContext(AmqpOrdinalFieldMap fieldMap, String outerclassName,
                                                   String methodBodyClassName, boolean versionConsistentFlag, AmqpVersionSet versionSet,
                                                   int indentSize, int tabSize)
    {
        String indent = Utils.createSpaces(indentSize);
        String tab = Utils.createSpaces(tabSize);
        StringBuffer sb = new StringBuffer();
        if (versionConsistentFlag)
        {
            sb.append(generateMethodBodyCall(fieldMap, methodBodyClassName, null, indentSize, tabSize));
        }
        else
        {
            boolean firstOverloadedMethodFlag = true;
            for (AmqpVersion thisVersion : versionSet)
            {
                sb.append(indent);
                if (!firstOverloadedMethodFlag)
                {
                    sb.append("else ");
                }
                sb.append("if (" + generateVersionCheck(thisVersion) + ")" + CR);
                sb.append(indent + "{" + CR);
                sb.append(generateMethodBodyCall(fieldMap, methodBodyClassName, thisVersion,
                                                 indentSize + tabSize, tabSize));
                sb.append(indent + "}" + CR);
                firstOverloadedMethodFlag = false;
            }
            sb.append(indent + "else" + CR);
            sb.append(indent + "{" + CR);
            sb.append(indent + tab + "std::stringstream ss;" + CR);
            sb.append(indent + tab + "ss << \"Call to " + outerclassName + "::" + methodBodyClassName +
                      "(u_int16_t" + generateMethodParameterList(fieldMap, 0, true, true, false) + ")\"" + CR);
            sb.append(indent + tab + tab + "<< \" is invalid for AMQP version \" << version.toString() << \".\";" + CR);
            sb.append(indent + tab + "throw new ProtocolVersionException(ss.str());" + CR);
            sb.append(indent + "}" + CR);
        }
        return sb.toString();
    }

    protected String generateMethodBodyCall(AmqpOrdinalFieldMap fieldMap, String methodBodyClassName,
                                            AmqpVersion version, int indentSize, int tabSize)
    {
        String indent = Utils.createSpaces(indentSize);
        String tab = Utils.createSpaces(tabSize);
        String namespace = version != null ? version.namespace() + "::" : "";
        StringBuffer sb = new StringBuffer(indent + "out->send( new AMQFrame(parent->getProtocolVersion(), channel," + CR);
        sb.append(indent + tab + "new " + namespace + methodBodyClassName + "( parent->getProtocolVersion()");
        sb.append(generateMethodParameterList(fieldMap, indentSize + (5 * tabSize), true, false, true));
        sb.append(" )));" + CR);
        return sb.toString();
    }

    protected String generateMethodBodyIncludes(AmqpClass thisClass, int indentSize)
    {
        StringBuffer sb = new StringBuffer();
        if (thisClass != null)
        {
            sb.append(generateClassMethodBodyInclude(thisClass, indentSize));
        }
        else
        {
            for (String thisClassName : getModel().getClassMap().keySet())
            {
                thisClass = getModel().getClassMap().get(thisClassName);
                sb.append(generateClassMethodBodyInclude(thisClass, indentSize));
            }
        }
        return sb.toString();
    }

    protected String generateClassMethodBodyInclude(AmqpClass thisClass, int indentSize)
    {
        StringBuffer sb = new StringBuffer();
        String indent = Utils.createSpaces(indentSize);
        for (String thisMethodName : thisClass.getMethodMap().keySet())
        {
            AmqpMethod method = thisClass.getMethodMap().get(thisMethodName);
            sb.append(indent + "#include <" + thisClass.getName() +
                      Utils.firstUpper(method.getName()) + "Body.h>" + CR);
        }
        return sb.toString();
    }

    // Methods used for generation of code snippets for MethodBody class generation

    protected String getIndex(AmqpOrdinalVersionMap indexMap, AmqpVersion version)
    {
        for (Integer thisIndex : indexMap.keySet())
        {
            AmqpVersionSet versionSet = indexMap.get(thisIndex);
            if (versionSet.contains(version))
            {
                return String.valueOf(thisIndex);
            }
        }
        throw new AmqpTemplateException("Unable to find index for version " + version);
    }

    protected String generateFieldDeclarations(AmqpFieldMap fieldMap, AmqpVersion version, int indentSize)
    {
        String indent = Utils.createSpaces(indentSize);
        StringBuffer sb = new StringBuffer();

        if (version == null)
        {
            version = getVersionSet().first();
        }
        AmqpOrdinalFieldMap ordinalFieldMap = fieldMap.getMapForVersion(version, true, this);
        for (Integer thisOrdinal : ordinalFieldMap.keySet())
        {
            String[] fieldDomainPair = ordinalFieldMap.get(thisOrdinal);
            sb.append(indent + fieldDomainPair[FIELD_CODE_TYPE] + " " + fieldDomainPair[FIELD_NAME] + ";" + CR);
        }
        return sb.toString();
    }

    protected String generateFieldGetMethods(AmqpFieldMap fieldMap, AmqpVersion version, int indentSize)
    {
        String indent = Utils.createSpaces(indentSize);
        StringBuffer sb = new StringBuffer();

        if (version == null)
        {
            version = getVersionSet().first();
        }
        AmqpOrdinalFieldMap ordinalFieldMap = fieldMap.getMapForVersion(version, true, this);
        for (Integer thisOrdinal : ordinalFieldMap.keySet())
        {
            String[] fieldDomainPair = ordinalFieldMap.get(thisOrdinal);
            sb.append(indent + "inline " + setRef(fieldDomainPair[FIELD_CODE_TYPE]) + " get" +
                      Utils.firstUpper(fieldDomainPair[FIELD_NAME]) + "() { return " +
                      fieldDomainPair[FIELD_NAME] + "; }" + CR);
        }
        return sb.toString();
    }

    protected String generatePrintMethodContents(AmqpFieldMap fieldMap, AmqpVersion version, int indentSize)
    {
        String indent = Utils.createSpaces(indentSize);
        StringBuffer sb = new StringBuffer();

        if (version == null)
        {
            version = getVersionSet().first();
        }
        AmqpOrdinalFieldMap ordinalFieldMap = fieldMap.getMapForVersion(version, true, this);
        boolean firstFlag = true;
        for (Integer thisOrdinal : ordinalFieldMap.keySet())
        {
            String[] fieldDomainPair = ordinalFieldMap.get(thisOrdinal);
            String cast = fieldDomainPair[FIELD_CODE_TYPE].compareTo("u_int8_t") == 0 ? "(int)" : "";
            sb.append(indent + "out << \"");
            if (!firstFlag)
            {
                sb.append("; ");
            }
            sb.append(fieldDomainPair[FIELD_NAME] + "=\" << " + cast + fieldDomainPair[FIELD_NAME] + ";" + CR);
            firstFlag = false;
        }
        return sb.toString();
    }

    protected String generateBodySizeMethodContents(AmqpFieldMap fieldMap, AmqpVersion version,
                                                    int indentSize)
    {
        String indent = Utils.createSpaces(indentSize);
        StringBuffer sb = new StringBuffer();
        ArrayList<String> bitFieldList = new ArrayList<String>();
        AmqpOrdinalFieldMap ordinalFieldMap = fieldMap.getMapForVersion(version, false, this);
        Iterator<Integer> oItr = ordinalFieldMap.keySet().iterator();
        int ordinal = 0;
        while (oItr.hasNext())
        {
            ordinal = oItr.next();
            String[] fieldDomainPair = ordinalFieldMap.get(ordinal);
            AmqpVersion thisVersion = version == null ? getVersionSet().first() : version;
            String domainType = getDomainType(fieldDomainPair[FIELD_CODE_TYPE], thisVersion);

            // Defer bit types by adding them to an array. When the first subsequent non-bit
            // type is encountered, then handle the bits. This allows consecutive bits to be
            // placed into the same byte(s) - 8 bits to the byte.
            if (domainType.compareTo("bit") == 0)
            {
                bitFieldList.add(fieldDomainPair[FIELD_NAME]);
            }
            else
            {
                if (bitFieldList.size() > 0) // Handle accumulated bit types (if any)
                {
                    sb.append(generateBitArrayBodySizeMethodContents(bitFieldList, ordinal, indentSize));
                }
                sb.append(indent + "size += " +
                          typeMap.get(domainType).size.replaceAll("#", fieldDomainPair[FIELD_NAME]) +
                          "; /* " + fieldDomainPair[FIELD_NAME] + ": " +
                          domainType + " */" + CR);
            }
        }
        if (bitFieldList.size() > 0) // Handle any remaining accumulated bit types
        {
            sb.append(generateBitArrayBodySizeMethodContents(bitFieldList, ordinal, indentSize));
        }
        return sb.toString();
    }

    protected String generateBitArrayBodySizeMethodContents(ArrayList<String> bitFieldList,
                                                            int ordinal, int indentSize)
    {
        int numBytes = ((bitFieldList.size() - 1) / 8) + 1;
        String indent = Utils.createSpaces(indentSize);
        StringBuffer sb = new StringBuffer();
        String comment = bitFieldList.size() == 1 ?
                         bitFieldList.get(0) + ": bit" :
                         "Combinded bits: " + bitFieldList;
        sb.append(indent + "size += " +
                  typeMap.get("bit").size.replaceAll("~", String.valueOf(numBytes)) +
                  "; /* " + comment + " */" + CR);
        bitFieldList.clear();
        return sb.toString();
    }

    protected String generateEncodeMethodContents(AmqpFieldMap fieldMap, AmqpVersion version,
                                                  int indentSize)
    {
        String indent = Utils.createSpaces(indentSize);
        StringBuffer sb = new StringBuffer();
        ArrayList<String> bitFieldList = new ArrayList<String>();
        AmqpOrdinalFieldMap ordinalFieldMap = fieldMap.getMapForVersion(version, false, this);
        Iterator<Integer> oItr = ordinalFieldMap.keySet().iterator();
        int ordinal = 0;
        while (oItr.hasNext())
        {
            ordinal = oItr.next();
            String[] fieldDomainPair = ordinalFieldMap.get(ordinal);
            AmqpVersion thisVersion = version == null ? getVersionSet().first() : version;
            String domainType = getDomainType(fieldDomainPair[FIELD_CODE_TYPE], thisVersion);

            // Defer bit types by adding them to an array. When the first subsequent non-bit
            // type is encountered, then handle the bits. This allows consecutive bits to be
            // placed into the same byte(s) - 8 bits to the byte.
            if (domainType.compareTo("bit") == 0)
            {
                bitFieldList.add(fieldDomainPair[FIELD_NAME]);
            }
            else
            {
                if (bitFieldList.size() > 0) // Handle accumulated bit types (if any)
                {
                    sb.append(generateBitEncodeMethodContents(bitFieldList, ordinal, indentSize));
                }
                sb.append(indent +
                          typeMap.get(domainType).encodeExpression.replaceAll("#", fieldDomainPair[FIELD_NAME]) +
                          "; /* " + fieldDomainPair[FIELD_NAME] + ": " + domainType + " */" + CR);
            }
        }
        if (bitFieldList.size() > 0) // Handle any remaining accumulated bit types
        {
            sb.append(generateBitEncodeMethodContents(bitFieldList, ordinal, indentSize));
        }

        return sb.toString();
    }

    protected String generateBitEncodeMethodContents(ArrayList<String> bitFieldList, int ordinal,
                                                     int indentSize)
    {
        int numBytes = ((bitFieldList.size() - 1) / 8) + 1;
        String indent = Utils.createSpaces(indentSize);
        String bitArrayName = "flags_" + ordinal;
        StringBuffer sb = new StringBuffer(indent + "u_int8_t " + bitArrayName +
                                           "[" + numBytes + "] = {0};" +
                                           (numBytes != 1 ? " /* All array elements will be initialized to 0 */" : "") +
                                           CR);
        for (int i = 0; i < bitFieldList.size(); i++)
        {
            int bitIndex = i % 8;
            int byteIndex = i / 8;
            sb.append(indent + bitArrayName + "[" + byteIndex + "] |= " + bitFieldList.get(i) +
                      " << " + bitIndex + "; /* " + bitFieldList.get(i) + ": bit */" + CR);
        }
        for (int i = 0; i < numBytes; i++)
        {
            sb.append(indent + "buffer.putOctet(" + bitArrayName + "[" + i + "]);" + CR);
        }
        bitFieldList.clear();
        return sb.toString();
    }

    protected String generateDecodeMethodContents(AmqpFieldMap fieldMap, AmqpVersion version,
                                                  int indentSize)
    {
        String indent = Utils.createSpaces(indentSize);
        StringBuffer sb = new StringBuffer();
        ArrayList<String> bitFieldList = new ArrayList<String>();
        AmqpOrdinalFieldMap ordinalFieldMap = fieldMap.getMapForVersion(version, false, this);
        Iterator<Integer> oItr = ordinalFieldMap.keySet().iterator();
        int ordinal = 0;
        while (oItr.hasNext())
        {
            ordinal = oItr.next();
            String[] fieldDomainPair = ordinalFieldMap.get(ordinal);
            AmqpVersion thisVersion = version == null ? getVersionSet().first() : version;
            String domainType = getDomainType(fieldDomainPair[FIELD_CODE_TYPE], thisVersion);

            // Defer bit types by adding them to an array. When the first subsequent non-bit
            // type is encountered, then handle the bits. This allows consecutive bits to be
            // placed into the same byte(s) - 8 bits to the byte.
            if (domainType.compareTo("bit") == 0)
            {
                bitFieldList.add(fieldDomainPair[FIELD_NAME]);
            }
            else
            {
                if (bitFieldList.size() > 0) // Handle accumulated bit types (if any)
                {
                    sb.append(generateBitDecodeMethodContents(bitFieldList, ordinal, indentSize));
                }
                sb.append(indent +
                          typeMap.get(domainType).decodeExpression.replaceAll("#", fieldDomainPair[FIELD_NAME]) +
                          "; /* " + fieldDomainPair[FIELD_NAME] + ": " + domainType + " */" + CR);
            }
        }
        if (bitFieldList.size() > 0) // Handle any remaining accumulated bit types
        {
            sb.append(generateBitDecodeMethodContents(bitFieldList, ordinal, indentSize));
        }

        return sb.toString();
    }

    protected String generateBitDecodeMethodContents(ArrayList<String> bitFieldList, int ordinal,
                                                     int indentSize)
    {
        int numBytes = ((bitFieldList.size() - 1) / 8) + 1;
        String indent = Utils.createSpaces(indentSize);
        String bitArrayName = "flags_" + ordinal;
        StringBuffer sb = new StringBuffer(indent + "u_int8_t " + bitArrayName +
                                           "[" + numBytes + "];" + CR);
        for (int i = 0; i < numBytes; i++)
        {
            sb.append(indent + bitArrayName + "[" + i + "] = buffer.getOctet();" + CR);
        }
        for (int i = 0; i < bitFieldList.size(); i++)
        {
            int bitIndex = i % 8;
            int byteIndex = i / 8;
            sb.append(indent + bitFieldList.get(i) + " = (1 << " + bitIndex + ") & " +
                      bitArrayName + "[" + byteIndex + "]; /* " + bitFieldList.get(i) +
                      ": bit */" + CR);
        }
        bitFieldList.clear();
        return sb.toString();
    }

    protected String generateFieldList(AmqpFieldMap fieldMap, AmqpVersion version, boolean defineFlag,
                                       boolean initializerFlag, int indentSize)
    {
        String indent = Utils.createSpaces(indentSize);
        StringBuffer sb = new StringBuffer();
        AmqpOrdinalFieldMap ordinalFieldMap = fieldMap.getMapForVersion(version, true, this);
        Iterator<Integer> oItr = ordinalFieldMap.keySet().iterator();
        while (oItr.hasNext())
        {
            int ordinal = oItr.next();
            String[] fieldDomainPair = ordinalFieldMap.get(ordinal);
            sb.append(indent + (defineFlag ? setRef(fieldDomainPair[FIELD_CODE_TYPE]) + " " : "") +
                      fieldDomainPair[FIELD_NAME] + (initializerFlag ? "(" + fieldDomainPair[FIELD_NAME] + ")" : "") +
                      (oItr.hasNext() ? "," : "") + CR);
        }
        return sb.toString();
    }

    protected String generateMethodParameterList(AmqpOrdinalFieldMap fieldMap, int indentSize,
                                                 boolean leadingCommaFlag, boolean fieldTypeFlag, boolean fieldNameFlag)
    {
        String indent = Utils.createSpaces(indentSize);
        StringBuffer sb = new StringBuffer();
        boolean first = true;
        Iterator<Integer> pItr = fieldMap.keySet().iterator();
        while (pItr.hasNext())
        {
            String[] field = fieldMap.get(pItr.next());
            if (first && leadingCommaFlag)
            {
                sb.append("," + (fieldNameFlag ? CR : " "));
            }
            if (!first || leadingCommaFlag)
            {
                sb.append(indent);
            }
            sb.append(
                    (fieldTypeFlag ? setRef(field[FIELD_CODE_TYPE]) : "") +
                    (fieldNameFlag ? " " + field[FIELD_NAME] : "") +
                    (pItr.hasNext() ? "," + (fieldNameFlag ? CR : " ") : ""));
            first = false;
        }
        return sb.toString();
    }

    protected String generateConstructor(AmqpClass thisClass, AmqpMethod method,
                                         AmqpVersion version, int indentSize, int tabSize)
    {
        String indent = Utils.createSpaces(indentSize);
        String tab = Utils.createSpaces(tabSize);
        StringBuffer sb = new StringBuffer();
        if (method.getFieldMap().size() > 0)
        {
            sb.append(indent + thisClass.getName() + Utils.firstUpper(method.getName()) + "Body(ProtocolVersion& version," + CR);
            sb.append(generateFieldList(method.getFieldMap(), version, true, false, 8));
            sb.append(indent + tab + ") :" + CR);
            sb.append(indent + tab + "AMQMethodBody(version)," + CR);
            sb.append(generateFieldList(method.getFieldMap(), version, false, true, 8));
            sb.append(indent + "{ }" + CR);
        }
        return sb.toString();
    }

    protected String generateServerOperationsInvoke(AmqpClass thisClass, AmqpMethod method,
                                                    AmqpVersion version, int indentSize, int tabSize)
    {
        String indent = Utils.createSpaces(indentSize);
        String tab = Utils.createSpaces(tabSize);
        StringBuffer sb = new StringBuffer();

        if (method.getServerMethodFlagMap().size() > 0) // At least one AMQP version defines this method as a server method
        {
            Iterator<Boolean> bItr = method.getServerMethodFlagMap().keySet().iterator();
            while (bItr.hasNext())
            {
                if (bItr.next()) // This is a server operation
                {
                    boolean fieldMapNotEmptyFlag = method.getFieldMap().size() > 0;
                    sb.append(indent + "inline void invoke(AMQP_ServerOperations& target, u_int16_t channel)" + CR);
                    sb.append(indent + "{" + CR);
                    sb.append(indent + tab + "target.get" + thisClass.getName() + "Handler()->" +
                              parseForReservedWords(Utils.firstLower(method.getName()),
                                                    thisClass.getName() + Utils.firstUpper(method.getName()) + "Body.invoke()") + "(channel");
                    if (fieldMapNotEmptyFlag)
                    {
                        sb.append("," + CR);
                        sb.append(generateFieldList(method.getFieldMap(), version, false, false, indentSize + 4 * tabSize));
                        sb.append(indent + tab + tab + tab + tab);
                    }
                    sb.append(");" + CR);
                    sb.append(indent + "}" + CR);
                }
            }
        }
        return sb.toString();
    }

    // Methods for generation of code snippets for amqp_methods.h/cpp files

    protected String generateMethodBodyIncludeList(AmqpModel model, int indentSize)
    {
        String indent = Utils.createSpaces(indentSize);
        StringBuffer sb = new StringBuffer();

        for (String thisClassName : model.getClassMap().keySet())
        {
            AmqpClass thisClass = model.getClassMap().get(thisClassName);
            for (String thisMethodName : thisClass.getMethodMap().keySet())
            {
                AmqpMethod method = thisClass.getMethodMap().get(thisMethodName);
                sb.append(indent + "#include \"" + thisClass.getName() + Utils.firstUpper(method.getName()) + "Body.h\"" + CR);
            }
        }

        return sb.toString();
    }

    protected String generateMethodBodyInstances(AmqpModel model, int indentSize)
    {
        String indent = Utils.createSpaces(indentSize);
        StringBuffer sb = new StringBuffer();

        for (String thisClassName : model.getClassMap().keySet())
        {
            AmqpClass thisClass = model.getClassMap().get(thisClassName);
            for (String thisMethodName : thisClass.getMethodMap().keySet())
            {
                AmqpMethod method = thisClass.getMethodMap().get(thisMethodName);
                sb.append(indent + "const " + thisClass.getName() + Utils.firstUpper(method.getName()) + "Body " +
                          Utils.firstLower(thisClass.getName()) + "_" + method.getName() + ";" + CR);
            }
        }

        return sb.toString();
    }

    protected String generateMethodBodyMapEntry(AmqpModel model, int indentSize)
    {
        String indent = Utils.createSpaces(indentSize);
        StringBuffer sb = new StringBuffer();

        for (AmqpVersion version : getVersionSet())
        {
            for (String thisClassName : model.getClassMap().keySet())
            {
                AmqpClass thisClass = model.getClassMap().get(thisClassName);
                for (String thisMethodName : thisClass.getMethodMap().keySet())
                {
                    AmqpMethod method = thisClass.getMethodMap().get(thisMethodName);
                    String namespace = method.isVersionConsistent(getVersionSet()) ? "" : version.namespace() + "::";
                    try
                    {
                        int classOrdinal = thisClass.getIndexMap().getOrdinal(version);
                        int methodOrdinal = method.getIndexMap().getOrdinal(version);
                        String methodModyClassName = namespace + thisClass.getName() + Utils.firstUpper(method.getName()) + "Body";
                        sb.append(indent + "insert(std::make_pair(createMapKey(" + classOrdinal + ", " +
                                  methodOrdinal + ", " + version.getMajor() + ", " + version.getMinor() +
                                  "), &createMethodBodyFn<" + methodModyClassName + ">));" + CR);
                    }
                    catch (AmqpTypeMappingException e)
                    {
                    } // ignore
                }
            }
        }

        return sb.toString();
    }

    // Helper functions

    private String generateVersionCheck(AmqpVersion version)
    {
        return "version.equals(" + version.getMajor() + ", " + version.getMinor() + ")";
    }

    private String generateVersionCheck(AmqpVersionSet versionSet)
    {
        StringBuffer sb = new StringBuffer();
        for (AmqpVersion v : versionSet)
        {
            if (!v.equals(versionSet.first()))
            {
                sb.append(" || ");
            }
            if (versionSet.size() > 1)
            {
                sb.append("(");
            }
            sb.append("version.equals(" + v.getMajor() + ", " + v.getMinor() + ")");
            if (versionSet.size() > 1)
            {
                sb.append(")");
            }
        }
        return sb.toString();
    }

    private String parseForReservedWords(String name, String context)
    {
        for (String cppReservedWord : cppReservedWords)
        {
            if (name.compareTo(cppReservedWord) == 0)
            {
                if (!quietFlag)
                {
                    System.out.println("WARNING: " + (context == null ? "" : context + ": ") +
                                       "Found XML method \"" + name + "\", which is a C++ reserved word. " +
                                       "Changing generated name to \"" + name + "_\".");
                }
                return name + "_";
            }
        }

        for (String cppCommonDefine : cppCommonDefines)
        {
            if (name.compareTo(cppCommonDefine) == 0)
            {
                if (!quietFlag)
                {
                    System.out.println("WARNING: " + (context == null ? "" : context + ": ") +
                                       "Found XML method \"" + name + "\", which may clash with commonly used defines within C++. " +
                                       "Changing generated name to \"" + name + "_\".");
                }
                return name + "_";
            }
        }

        return name;
    }

    private String setRef(String codeType)
    {
        if (codeType.compareTo("string") == 0 ||
            codeType.compareTo("FieldTable") == 0)
        {
            return "const " + codeType + "&";
        }
        return codeType;
    }

    private String camelCaseName(String name, boolean upperFirstFlag)
    {
        StringBuffer ccn = new StringBuffer();
        String[] toks = name.split("[-_.\\ ]");
        for (int i = 0; i < toks.length; i++)
        {
            StringBuffer b = new StringBuffer(toks[i]);
            if (upperFirstFlag || i > 0)
            {
                b.setCharAt(0, Character.toUpperCase(toks[i].charAt(0)));
            }
            ccn.append(b);
        }
        return ccn.toString();
    }

    public static Factory<CppGenerator> _factoryInstance = new Factory<CppGenerator>()
    {

        public CppGenerator newInstance()
        {
            return new CppGenerator();
        }
    };

    public static Factory<CppGenerator> getFactory()
    {
        return _factoryInstance;
    }

    void processModelTemplate(NamedTemplate template, AmqpVersion version)
    {
        //To change body of implemented methods use File | Settings | File Templates.
    }
    public String getNativeType(String type)
    {
        throw new UnsupportedOperationException();
    }

    public String getEncodingType(String type)
    {
        throw new UnsupportedOperationException();
    }

}
