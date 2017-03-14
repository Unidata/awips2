/**
 * 
 */
package com.raytheon.wes2bridge.configuration;

import java.io.File;

import javax.xml.bind.JAXBException;

import com.raytheon.wes2bridge.common.configuration.Wes2BridgeCase;
import com.raytheon.wes2bridge.configuration.jaxb.Wes2BridgeJaxbManager;

/**
 * A command line utility that can be used to retrieve individual values from configuration.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * ?            ?          bkowal      Initial Creation.
 * Aug 14, 2014 3521       bkowal      Updated to use Wes2BridgeCase.
 * 
 * </pre>
 * 
 * @author bkowal
 * @version 1.0
 */
public class ConfigurationUtility {
    private static final String FIELD_NAME = "-name";

    private static final String FIELD_ARCHIVE = "-archiveRoot";

    private static final String FIELD_DBPORT = "-databasePort";

    private static final String FIELD_HTTPPORT = "-httpPort";

    private static final String FIELD_JMSPORT = "-jmsPort";

    /**
     * @param args
     */
    public static void main(String[] args) {
        if (args.length != 2) {
            System.out
                    .println("Error: both a configuration file and a field must be specified.");
            System.exit(-1);
        }

        Wes2BridgeCase wes2BridgeCase = null;
        try {
            wes2BridgeCase = Wes2BridgeJaxbManager.toWes2BridgeCase(new File(
                    args[0]));
        } catch (JAXBException e) {
            e.printStackTrace();
            System.out
                    .println("FAILED TO READ THE SPECIFIED CONFIGURATION FILE: "
                            + args[0] + "!");
            System.exit(-1);
        }

        final String field = args[1];
        if (field.equals(FIELD_NAME)) {
            System.out.print(wes2BridgeCase.getName());
        } else if (field.equals(FIELD_ARCHIVE)) {
            System.out.print(wes2BridgeCase.getDataArchiveRoot());
        } else if (field.equals(FIELD_DBPORT)) {
            System.out.print(wes2BridgeCase.getDatabasePort());
        } else if (field.equals(FIELD_HTTPPORT)) {
            System.out.print(wes2BridgeCase.getEdexHttpPort());
        } else if (field.equals(FIELD_JMSPORT)) {
            System.out.print(wes2BridgeCase.getJmsPort());
        }
        System.exit(0);
    }
}