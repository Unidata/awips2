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
package com.raytheon.uf.common.registry.ebxml.version;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.concurrent.ConcurrentHashMap;

import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.URIResolver;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

import com.raytheon.uf.common.localization.FileUpdatedMessage;
import com.raytheon.uf.common.localization.ILocalizationFileObserver;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * 
 * Uses XSLT files to transform other versions of Registry classes into the
 * current version.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 3/2/2014     2789        dhladky    Initial implementation
 * </pre>
 * 
 * @author dhladky
 * @version 1
 */

public class VersionTransformer implements ILocalizationFileObserver {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(VersionTransformer.class);
    
    /** Path to XSLT file PREFIX **/
    private static final String XSLT_FILE_PREFIX = IPathManager.SEPARATOR
            + "registry" + IPathManager.SEPARATOR + "versions"
            + IPathManager.SEPARATOR;

    /** XSLT file SUFFIX **/
    private static final String XSLT_FILE_SUFFIX = ".xsl";

    /** Factory for generating XSLT transformations */
    private static final TransformerFactory factory = TransformerFactory
            .newInstance();

    /** Map of XSLT files for quick lookup **/
    private static ConcurrentHashMap<String, File> styleSheetFiles;

    /** Singleton instance of this class */
    private static VersionTransformer instance = new VersionTransformer();
    
    /** Helps resolve path to XSLT files **/
    private static Resolver resolver;
    
    /** Version target for transformation **/
    private final static String target_version_property = "target_version";
    
    /** Content version for transformation **/
    private final static String content_version_property = "content_version";

    /** Private Constructor **/
    private VersionTransformer() {
        styleSheetFiles = new ConcurrentHashMap<String, File>();
        resolver = new Resolver();
    }
    
    /** Public test constructor **/
    public VersionTransformer(String path) {
        styleSheetFiles = new ConcurrentHashMap<String, File>();
        resolver = new Resolver(path);
    }

    /**
     * Get an instance of this singleton.
     */
    public static VersionTransformer getInstance() {
        return instance;
    }
    
   /**
    *
    * Transform the content into the given classes current XML version.
    * 
    * @param content
    * @param className
    * @param targetVersion
    * @param contentVersion
    * @return
    * @throws Exception
    */
    public String transform(String content, String className, String targetVersion, String contentVersion) throws Exception {

        StringWriter outputWriter = new StringWriter();
        File styleSheet = getStyleSheet(className);
        FileReader styleSheetReader = new FileReader(styleSheet);
        String newXML = null;

        Transformer transformer = factory.newTransformer(new StreamSource(
                styleSheetReader));
        // Sets version you want to go to, 
        // Will use this in the future for scaling our XSLT to convert to target versions.
        transformer.setParameter(target_version_property, targetVersion);
        transformer.setParameter(content_version_property, contentVersion);
        // read imported sheets from the same directory, use resolver
        factory.setURIResolver(resolver);

        transformer.transform(new StreamSource(new StringReader(content)),
                new StreamResult(outputWriter));
        // Make string of new XML
        newXML = outputWriter.toString();

        if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
            statusHandler.info("Transform successful: " + className);
        }

        return newXML;
    }

    /**
     * Looks up and locates the styleSheet in localization
     * 
     * @param className
     * @return
     */
    private File getStyleSheet(String className) {
        
        String path = className + XSLT_FILE_SUFFIX;
        File styleSheet = styleSheetFiles.get(path);

        if (styleSheet == null) {
            styleSheet = resolver.getInitialFile(path);
            styleSheetFiles.put(path, styleSheet);
        }

        return styleSheet;
    }

    /**
     * 
     * Appends the base directory to the URI for the XSL templates
     * 
     * <pre>
     *
     * SOFTWARE HISTORY
     *
     * Date         Ticket#    Engineer    Description
     * ------------ ---------- ----------- --------------------------
     * Mar 24, 2014            dhladky     Initial creation
     *
     * </pre>
     *
     * @author dhladky
     * @version 1.0
     */
    public class Resolver implements URIResolver {
        
        private String testPath = null;
        /**
         * public construct
         */
        public Resolver() {

        }
        
        public Resolver(String mainPath) {
            this.testPath = mainPath;
        }

        /**
         * Append the base_path to the template file and returns the files as a
         * stream.
         */
        public Source resolve(String href, String base) {

            File file = null;

            if (testPath != null) {
                StringBuffer path = new StringBuffer(this.testPath);
                path.append(href);
                file = styleSheetFiles.get(path.toString());

                if (file == null) {
                    file = new File(path.toString());
                    styleSheetFiles.put(path.toString(), file);
                }

            } else {

                String path = XSLT_FILE_PREFIX + href;
                file = styleSheetFiles.get(path);

                // Add templates loaded by the Transformer to the cache as well
                if (file == null) {
                    file = getLocalizationFile(path);
                    styleSheetFiles.put(path, file);
                }
            }

            if (file != null && file.exists()) {
                return new StreamSource(file);
            } else {
                return null;
            }
        }
        
        /**
         * Used for the top level, initial XSL call
         * @param name
         * @return
         */
        public File getInitialFile(String name) {
            
            File file = null;
            
            if (testPath != null) {
                StringBuffer path = new StringBuffer(this.testPath);
                path.append(name);
                file = new File(path.toString());
            } else {
                String path = XSLT_FILE_PREFIX+name;
                file = getLocalizationFile(path);
            }
            
            if (file != null && file.exists()) {
                return file;
            } else {
                return null;
            }
        }
    }
    
    /**
     * Gets a file from localization
     * @param name
     * @return
     */
    private static File getLocalizationFile(String path) {
        
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext lc = pm.getContext(LocalizationType.EDEX_STATIC, LocalizationLevel.BASE);
        LocalizationFile lf = pm.getLocalizationFile(lc, path);
        lf.addFileUpdatedObserver(getInstance());
        
        return lf.getFile();
    }
   
    @Override
    public void fileUpdated(FileUpdatedMessage message) {

        if (message.getFileName().startsWith(XSLT_FILE_PREFIX)) {
            try {
                String key = message.getFileName();
                
                if (styleSheetFiles.containsKey(key)) {
                    // update the map
                    File file = resolver.getInitialFile(key);
                    if (file != null) {
                        styleSheetFiles.replace(key, file);
                    }
                }

            } catch (Exception e) {
                statusHandler.handle(Priority.WARN,
                        "VersionTransformer: " + message.getFileName()
                                + " File couldn't be updated.", e);
            }
        }
    }


    /**
     * Used for testing only!
     * @param argv
     * @throws TransformerException
     * @throws IOException
     */
    public static void main(String[] argv) throws TransformerException, IOException {
        if (argv.length != 3) {

            System.exit(1);
        }

        @SuppressWarnings("unused")
        VersionTransformer vt = new VersionTransformer(argv[2]);
        factory.setURIResolver(VersionTransformer.resolver);
        
        StringWriter outputWriter = new StringWriter();

        FileReader styleSheetReader = new FileReader(new File(argv[1]));

        Transformer transformer = factory.newTransformer(new StreamSource(
                styleSheetReader));
                
        transformer.transform(new StreamSource(
                new FileReader(new File(argv[0]))), new StreamResult(
                outputWriter));
        System.out.println(outputWriter.toString());

    }
}
