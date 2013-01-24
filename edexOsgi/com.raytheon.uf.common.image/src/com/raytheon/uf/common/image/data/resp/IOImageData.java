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
package com.raytheon.uf.common.image.data.resp;

import java.awt.image.BufferedImage;

/* 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 27, 2009            mschenke     Initial creation
 *
 * </pre>
 *
 * @author mschenke
 */
/**
 * Image data used for GLImage, which expects a RenderedImage
 * 
 * @version 1.0
 */
public class IOImageData extends ImageData {
	private BufferedImage image;

	private int border;

	private String name;

	public IOImageData(BufferedImage image, int bord, String name) {
		this.image = image;
		this.border = bord;
		this.name = name;
	}

	public void setImage(BufferedImage image) {
		this.image = image;
	}

	public BufferedImage getImage() {
		return image;
	}

	public int getBorder() {
		return border;
	}

	public String getName() {
		return name;
	}

}
