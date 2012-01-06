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
package com.raytheon.edex.plugin.data;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.postgis.PGgeometry;
import org.postgis.Point;

import com.raytheon.uf.common.pointdata.spatial.ObStation;

/**
 * JUnit test case for {@link com.raytheon.edex.plugin.data.ObsStation} class.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 19June2007   343         MW Fegan    Initial creation.
 * 
 * </pre>
 * 
 * @author mfegan
 * @version 1
 */

public class TestObsStation {
	/* test data values */
	/* represents POINT(-95.76666666666667 41.266666666666666) */
	private byte[] geometry = { 48, 49, 48, 49, 48, 48, 48, 48, 48, 48, 49, 49,
			49, 49, 49, 49, 49, 49, 49, 49, 70, 49, 53, 55, 67, 48, 50, 50, 50,
			50, 50, 50, 50, 50, 50, 50, 65, 50, 52, 52, 52, 48 };
	private Point point = null;
	/* the test object */
	private ObStation station = null;

	/**
	 * @throws java.lang.Exception
	 */
	@Before
	public void setUp() throws Exception {
		station = new ObStation();
		point = (Point) PGgeometry.geomFromString(new String(geometry));
	}

	/**
	 * @throws java.lang.Exception
	 */
	@After
	public void tearDown() throws Exception {
		station = null;
	}

	/**
	 * Tests the operation of the
	 * {@link com.raytheon.uf.common.pointdata.spatial.ObStation#setGeometryWKB(byte[])}
	 * method. This method sets both the {@code geometryWKB} and
	 * {@code geometry} attributes.
	 */
	@Test
	public void basicGeometryWKBTest() {
		// station.setGeometryWKB(null);
		assertNotNull("Checking geometry not null", station.getGeometry());
		assertEquals("Checking geometry value", point.toString(), station
				.getGeometry().toString());
		// assertNotNull("Checking geomertyWKB not
		// null",station.getGeometryWKB());
		// assertEquals("Checking geometryWKB value", geometry, station
		// .getGeometryWKB());

	}
}
