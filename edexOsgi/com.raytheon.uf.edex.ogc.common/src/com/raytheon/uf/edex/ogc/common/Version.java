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
package com.raytheon.uf.edex.ogc.common;

import java.io.Serializable;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Version object to track versions of OGC services
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 2011            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class Version implements Comparable<Version>, Serializable {

	private transient static final long serialVersionUID = -7496995218051649871L;

	@DynamicSerializeElement
	@XmlAttribute
	protected int major = 0;

	@DynamicSerializeElement
	@XmlAttribute
	protected int minor = 0;

	@DynamicSerializeElement
	@XmlAttribute
	protected int micro = 0;

	@DynamicSerializeElement
	@XmlAttribute
	protected String qualifier;

	protected transient Pattern pattern = Pattern
			.compile("^([0-9]+)(\\.([0-9]+)(\\.([0-9]+)(\\.([^\\.,]+))?)?)?$");

	public Version() {
	}

	public Version(Version pv) {
		this(pv.major, pv.minor, pv.micro, pv.qualifier);
	}

	public Version(int major, int minor, int micro) {
		this(major, minor, micro, null);
	}

	public Version(int major, int minor, int micro, String qualifier) {
		this.major = major;
		this.micro = micro;
		this.minor = minor;
		this.qualifier = qualifier;
	}

	public Version(String version) throws InvalidVersionException {
		fromString(version);
	}

	protected void fromString(String version) throws InvalidVersionException {
		Matcher m = pattern.matcher(version.trim());
		if (m.matches()) {
			major = parseInt(m.group(1));
			minor = parseInt(m.group(3));
			micro = parseInt(m.group(5));
			qualifier = m.group(7);
		} else {
			throw new InvalidVersionException("Invalid version string: '"
					+ version + "'");
		}
	}

    public Version majorMinorOnly() {
        return new Version(this.major, this.minor, 0);
    }

	protected int parseInt(String str) {
		int rval = 0;
		if (str != null) {
			rval = Integer.parseInt(str);
		}
		return rval;
	}

	public int getMajor() {
		return major;
	}

	public void setMajor(int major) {
		this.major = major;
	}

	public int getMinor() {
		return minor;
	}

	public void setMinor(int minor) {
		this.minor = minor;
	}

	public int getMicro() {
		return micro;
	}

	public void setMicro(int micro) {
		this.micro = micro;
	}

	public String getQualifier() {
		return qualifier;
	}

	public void setQualifier(String qualifier) {
		this.qualifier = qualifier;
	}

	public String toString() {
		String rval = String.format("%d.%d.%d", major, minor, micro);
		if (this.qualifier != null && !this.qualifier.isEmpty()) {
			rval += "." + qualifier;
		}
		return rval;
	}

	@Override
	public int compareTo(Version o) {
		if (o == null) {
			return 1;
		}
		if (o == this) {
			return 0;
		}
        int res = major - o.major;
		if (res != 0) {
			return res;
		}
		res = minor - o.minor;
		if (res != 0) {
			return res;
		}
		res = micro - o.micro;
		if (res != 0) {
			return res;
		}
		if (qualifier == null) {
			if (o.qualifier != null) {
				return -1;
			}
		} else if (o.qualifier == null) {
			return 1;
		} else {
			return qualifier.compareTo(o.qualifier);
		}

		return 0;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + major;
		result = prime * result + micro;
		result = prime * result + minor;
		result = prime * result
				+ ((qualifier == null) ? 0 : qualifier.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		Version other = (Version) obj;
		if (major != other.major)
			return false;
		if (micro != other.micro)
			return false;
		if (minor != other.minor)
			return false;
		if (qualifier == null) {
			if (other.qualifier != null)
				return false;
		} else if (!qualifier.equals(other.qualifier))
			return false;
		return true;
	}

    /**
     * @param other
     * @return true if this and other have the same major and minor versions
     */
    public boolean equalsMajorMinor(Version other) {
        if (other == null) {
            return false;
        }
        if (major != other.major)
            return false;
        if (minor != other.minor)
            return false;
        return true;
    }

}
