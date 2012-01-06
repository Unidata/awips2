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

package com.raytheon.viz.core.gl;

import org.eclipse.swt.opengl.GLCanvas;

/**
 * 
 * Caches the initial GLCanvas
 * 
 * This is not ideal, since the SWT resources for the first canvas are always
 * left open after the map is disposed, but due to limitations in JOGL and
 * Eclipse, it is necessary.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 10, 2007            chammack    Initial Creation.	
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class GLCanvasCache {

	private static GLCanvasCache instance;

	private GLCanvas canvas;

	public static synchronized GLCanvasCache getInstance() {
		if (instance == null) {
			instance = new GLCanvasCache();
		}
		return instance;
	}

	/**
	 * @return the canvas
	 */
	public GLCanvas getCanvas() {
		return canvas;
	}

	/**
	 * @param canvas
	 *            the canvas to set
	 */
	public void setCanvas(GLCanvas canvas) {
		this.canvas = canvas;
	}

}
