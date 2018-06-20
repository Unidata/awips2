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
package com.raytheon.edex.plugin.gfe.sample;

import java.util.Arrays;
import java.util.List;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

import com.raytheon.uf.common.dataplugin.gfe.sample.SampleData;
import com.raytheon.uf.common.dataplugin.gfe.sample.SampleId;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Test the SampleData class
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Apr 21, 2008				rbell	Initial creation
 * Jul 25, 2013 2208       njensen     Moved to tests project
 * 
 * </pre>
 * 
 * @author rbell
 * @version 1.0
 */

// TODO fix?
@Ignore
public class SampleDataTest {

    private String testFileContents;

    private String testLegacyFileContents;

    private SampleId testSampleId;

    private List<Coordinate> testCoordinates;

    // private File testFile;

    // private File testLegacyFile;

    /**
     * @throws java.lang.Exception
     */
    @Before
    public void setUp() throws Exception {
        this.testFileContents = "LINESTRING (-97.1128 47.556, -94.0116 47.0605, -101.024 47.4767, -102.702 44.4854, -96.6437 42.0843, -94.0336 44.9386, -99.0419 44.77, -94.0612 42.0893, -97.2013 43.4235, -100.573 42.0067)";
        this.testLegacyFileContents = "10\n-97.1128 47.556\n-94.0116 47.0605\n-101.024 47.4767\n-102.702 44.4854\n-96.6437 42.0843\n-94.0336 44.9386\n-99.0419 44.77\n-94.0612 42.0893\n-97.2013 43.4235\n-100.573 42.0067";
        this.testSampleId = new SampleId("testSampleIdString");
        this.testCoordinates = Arrays.asList(new Coordinate(1.1, 2.2),
                new Coordinate(3.3, 4.4), new Coordinate(5.5, 6.6));

        // this.testFile = File.createTempFile("SampleDataTest.java", "file");
        // try {
        // FileWriter fstream = new FileWriter(this.testFile);
        // BufferedWriter out = new BufferedWriter(fstream);
        // out.write(this.testFileContents);
        // out.close();
        // } catch (Exception e) {
        // System.err.println("Error: " + e.getMessage());
        // }
        //
        // this.testLegacyFile = File.createTempFile("SampleDataTest.java",
        // "legacyFile");
        // try {
        // FileWriter fstream = new FileWriter(this.testLegacyFile);
        // BufferedWriter out = new BufferedWriter(fstream);
        // String[] lines = this.testLegacyFileContents.split("\\n");
        // for(String thisLine : lines){
        // out.write(thisLine + "\n");
        // }
        // out.close();
        // } catch (Exception e) {
        // System.err.println("Error: " + e.getMessage());
        // }
    }

    /**
     * Test method for
     * {@link com.raytheon.uf.common.dataplugin.gfe.sample.SampleData#hashCode()}
     * .
     */
    @Test
    public void testHashCode() {
        SampleData test1 = SampleData.createSampleDataFromLegacyFileContents(
                this.testLegacyFileContents, this.testSampleId);
        test1.hashCode();
    }

    /**
     * Test method for
     * {@link com.raytheon.uf.common.dataplugin.gfe.sample.SampleData#SampleData()}
     * .
     */
    @Test
    public void testSampleData() {
        new SampleData();
    }

    /**
     * Test method for
     * {@link com.raytheon.uf.common.dataplugin.gfe.sample.SampleData#SampleData(com.raytheon.uf.common.dataplugin.gfe.sample.SampleId, java.util.ArrayList)}
     * .
     */
    @Test
    public void testSampleDataSampleIdArrayListOfCoordinate() {
        new SampleData(this.testSampleId, this.testCoordinates);
    }

    /**
     * Test method for
     * {@link com.raytheon.uf.common.dataplugin.gfe.sample.SampleData#SampleData(java.io.File)}
     * .
     */
    @Test
    public void testSampleDataFile() {
        // Assert.assertEquals(new SampleData(this.testFile), new
        // SampleData(this.testLegacyFile));
    }

    /**
     * Test method for
     * {@link com.raytheon.uf.common.dataplugin.gfe.sample.SampleData#SampleData(java.io.File, com.raytheon.uf.common.dataplugin.gfe.sample.SampleId)}
     * .
     */
    @Test
    public void testSampleDataFileSampleId() {
        // Assert.assertEquals(new SampleData(this.testFile, this.testSampleId),
        // new SampleData(this.testLegacyFile, this.testSampleId));
    }

    /**
     * Test method for
     * {@link com.raytheon.uf.common.dataplugin.gfe.sample.SampleData#SampleData(com.raytheon.uf.common.dataplugin.gfe.sample.SampleData)}
     * .
     */
    @Test
    public void testSampleDataSampleData() {
        SampleData test1 = new SampleData(this.testSampleId,
                this.testCoordinates);
        SampleData test2 = new SampleData(test1);
        Assert.assertEquals(test1, test2);
    }

    /**
     * Test method for
     * {@link com.raytheon.uf.common.dataplugin.gfe.sample.SampleData#createSampleDataFromLineString(java.lang.String)}
     * .
     */
    @Test
    public void testCreateSampleDataFromLineStringString() {
        SampleData.createSampleDataFromLineString(this.testFileContents);
    }

    /**
     * Test method for
     * {@link com.raytheon.uf.common.dataplugin.gfe.sample.SampleData#createSampleDataFromLegacyFileContents(java.lang.String)}
     * .
     */
    @Test
    public void testCreateSampleDataFromLegacyFileContentsString() {
        SampleData
                .createSampleDataFromLegacyFileContents(this.testLegacyFileContents);
    }

    /**
     * Test method for
     * {@link com.raytheon.uf.common.dataplugin.gfe.sample.SampleData#createSampleDataFromLineString(java.lang.String, com.raytheon.uf.common.dataplugin.gfe.sample.SampleId)}
     * .
     */
    @Test
    public void testCreateSampleDataFromLineStringStringSampleId() {
        SampleData.createSampleDataFromLineString(this.testFileContents,
                this.testSampleId);
    }

    /**
     * Test method for
     * {@link com.raytheon.uf.common.dataplugin.gfe.sample.SampleData#createSampleDataFromLegacyFileContents(java.lang.String, com.raytheon.uf.common.dataplugin.gfe.sample.SampleId)}
     * .
     */
    @Test
    public void testCreateSampleDataFromLegacyFileContentsStringSampleId() {
        SampleData.createSampleDataFromLegacyFileContents(
                this.testLegacyFileContents, this.testSampleId);
    }

    /**
     * Test method for
     * {@link com.raytheon.uf.common.dataplugin.gfe.sample.SampleData#getSampleId()}
     * .
     */
    @Test
    public void testGetSampleId() {
        SampleData test1 = new SampleData(this.testSampleId,
                this.testCoordinates);
        Assert.assertEquals(test1.getSampleId(), this.testSampleId);
    }

    /**
     * Test method for
     * {@link com.raytheon.uf.common.dataplugin.gfe.sample.SampleData#getPoints()}
     * .
     */
    @Test
    public void testGetPoints() {
        SampleData test1 = new SampleData(this.testSampleId,
                this.testCoordinates);
        Assert.assertEquals(test1.getPoints(), this.testCoordinates);
    }

    /**
     * Test method for
     * {@link com.raytheon.uf.common.dataplugin.gfe.sample.SampleData#equals(java.lang.Object)}
     * .
     */
    @Test
    public void testEqualsObject() {
        SampleData test1 = SampleData.createSampleDataFromLegacyFileContents(
                this.testLegacyFileContents, this.testSampleId);
        SampleData test2 = new SampleData(test1);
        SampleData test3 = new SampleData(this.testSampleId,
                this.testCoordinates);
        Assert.assertEquals(test1, test2);
        Assert.assertFalse(test1.equals(test3));
    }

    /**
     * Test method for
     * {@link com.raytheon.uf.common.dataplugin.gfe.sample.SampleData#toString()}
     * .
     */
    @Test
    public void testToString() {
        new SampleData(this.testSampleId, this.testCoordinates).toString();
    }

    /**
     * Test method for
     * {@link com.raytheon.uf.common.dataplugin.gfe.sample.SampleData#toLineString()}
     * .
     */
    @Test
    public void testToLineString() {
        SampleData test1 = SampleData.createSampleDataFromLegacyFileContents(
                this.testLegacyFileContents, this.testSampleId);
        SampleData test2 = SampleData.createSampleDataFromLineString(
                this.testFileContents, this.testSampleId);
        Assert.assertEquals(test1, test2);
        Assert.assertEquals(test1.getGeometry().toText(), this.testFileContents);
        Assert.assertEquals(test2.getGeometry().toText(), this.testFileContents);
    }

    /**
     * Test method for
     * {@link com.raytheon.uf.common.dataplugin.gfe.sample.SampleData#clone()}.
     */
    @Test
    public void testClone() {
        SampleData test1 = new SampleData(this.testSampleId,
                this.testCoordinates);
        SampleData test2 = test1.clone();
        Assert.assertEquals(test1, test2);
    }

}
