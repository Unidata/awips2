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

package com.raytheon.edex.msg;

import java.io.File;
import java.io.StringWriter;
import java.util.ArrayList;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.edex.util.Util;
import com.raytheon.uf.edex.core.EdexException;

/**
 * Container class representing the output of a program launched by AutoBldSrv.
 * Includes methods for generating and saving XML.
 * 
 * <pre>
 * 
 * 
 * SOFTWARE HISTORY
 *    
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 01Sept2006       17      Phillippe   Initial Creation	
 * 12Feb2007       TO5      MW Fegan    Modified XML creation as separate methods.
 * 02Oct2007       345      MW Fegan    Moved from com.raytheon.edex.adapterSrv.
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */

@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public class ProgramOutput {

    @XmlElement
    private String executionTime;

    @XmlElement
    private String programName;

    @XmlElement
    private String programOutput;

    /**
     * Constructor.
     */
    public ProgramOutput() {

    }

    /**
     * Initializing constructor.
     * 
     * @param executionTime
     *            time required to edexute program
     * @param program
     *            program command line as an Array
     * @param programOutput
     *            output from running program
     */
    public ProgramOutput(float executionTime, ArrayList<String> program,
            String programOutput) {

        this.programName = "";
        for (int i = 0; i < program.size(); i++) {
            this.programName += program.get(i) + " ";
        }
        this.programOutput = programOutput;
        this.executionTime = Util.formatDate(executionTime);

    }

    /**
     * Outputs the XML representation of this class to the specified file.
     * 
     * @param outFile
     *            the file to get the XML
     * 
     * @throws EdexException
     *             in the event of error.
     */
    public void writeXML(String outFile) throws EdexException {
        File theXMLFile = new File(outFile);
        try {
            theXMLFile.createNewFile();
            // IBindingFactory bfact = BindingDirectory
            // .getFactory(this.getClass());
            // IMarshallingContext mctx = bfact.createMarshallingContext();
            // mctx.setIndent(1);
            // mctx.setOutput(new FileWriter(theXMLFile));
            // mctx.marshalDocument(this);
        } catch (Exception e) {
            throw new EdexException("Error creating XML document", e);
        }
    }

    /**
     * Converts the class to its XML version.
     * 
     * @return the XML string
     * 
     * @throws EdexException
     *             in the event of error.
     */
    public String toXML() throws EdexException {
        try {
            StringWriter outWriter = new StringWriter();

            // IBindingFactory bfact = BindingDirectory
            // .getFactory(this.getClass());
            // IMarshallingContext mctx = bfact.createMarshallingContext();
            // mctx.setIndent(3, Util.EOL, ' ');
            // mctx.marshalDocument(this, null, null, outWriter);
            return outWriter.toString();
        } catch (Exception e) {
            throw new EdexException("Error creating XML document", e);
        }
    }

    /**
     * @return the executionTime
     */
    public String getExecutionTime() {
        return executionTime;
    }

    /**
     * @param executionTime
     *            the executionTime to set
     */
    public void setExecutionTime(String executionTime) {
        this.executionTime = executionTime;
    }

    /**
     * @return the programName
     */
    public String getProgramName() {
        return programName;
    }

    /**
     * @param programName
     *            the programName to set
     */
    public void setProgramName(String programName) {
        this.programName = programName;
    }

    /**
     * @return the programOutput
     */
    public String getProgramOutput() {
        return programOutput;
    }

    /**
     * @param programOutput
     *            the programOutput to set
     */
    public void setProgramOutput(String programOutput) {
        this.programOutput = programOutput;
    }
}
