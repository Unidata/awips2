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
package com.raytheon.rcm.config;

/** 
 * Describes a partial WMO heading to be used when send a radar product. The 
 * rest comes from the global "region code" in 
 * {@link com.raytheon.rcm.config.Configuration} and the actual product's timestamp.
 * 
 */
public class ProductDistributionInfo {
	protected String nnn;
	protected String ttaai;

	public ProductDistributionInfo() {
		// TODO: non-null values not valid...
	}
	
	public ProductDistributionInfo(String nnn, String ttaai) {
		this.nnn = nnn;
		this.ttaai = ttaai;
	}

	public String getNnn() {
		return nnn;
	}

	public void setNnn(String nnn) {
		this.nnn = nnn;
	}

	public String getTtaai() {
		return ttaai;
	}

	public void setTtaai(String ttaai) {
		this.ttaai = ttaai;
	}

	@Override
	public boolean equals(Object obj) {
		try {
			ProductDistributionInfo other = (ProductDistributionInfo) obj;
			return nnn.equals(other.nnn) && ttaai.equals(other.ttaai);
		} catch (ClassCastException e) {
			return false;
		}
	}

	@Override
	public int hashCode() {
		return nnn.hashCode() ^ ttaai.hashCode();
	}
}
