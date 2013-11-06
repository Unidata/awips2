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
package com.raytheon.uf.edex.wfs.request;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

import javax.xml.namespace.QName;

import net.opengis.wfs.v_1_1_0.DescribeFeatureTypeType;

/**
 * Request wrapper for WFS describe feature type request
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * April 2011            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class DescFeatureTypeReq extends WfsRequest {

	protected List<QualifiedName> typenames;

	protected String outputformat = "text/xml; subtype=gml/3.1.1";

	/**
	 * @param type
	 */
	public DescFeatureTypeReq() {
		super(Type.DescribeFeature);
		this.typenames = new LinkedList<QualifiedName>();
	}

	public DescFeatureTypeReq(List<QualifiedName> typenames) {
		super(Type.DescribeFeature);
		this.typenames = typenames;
	}

	/**
	 * @param obj
	 * @return
	 */
	public DescFeatureTypeReq(DescribeFeatureTypeType req) {
		super(Type.DescribeFeature);
		setRawrequest(req);
		String outputFormat = req.getOutputFormat();
		if (outputFormat != null && !outputFormat.isEmpty()) {
			setOutputformat(outputFormat);
		}
		List<QName> typeName = req.getTypeName();
		if (typeName != null) {
			for (QName n : typeName) {
				QualifiedName qname = new QualifiedName(n.getNamespaceURI(),
						n.getLocalPart(), n.getPrefix());
				addTypename(qname);
			}
		}
	}

    /**
     * @param obj
     */
    public DescFeatureTypeReq(
            net.opengis.wfs.v_2_0_0.DescribeFeatureTypeType req) {
        super(Type.DescribeFeature);
        setRawrequest(req);
        String outputFormat = req.getOutputFormat();
        if (outputFormat != null && !outputFormat.isEmpty()) {
            setOutputformat(outputFormat);
        }
        List<QName> typeName = req.getTypeName();
        if (typeName != null) {
            for (QName n : typeName) {
                QualifiedName qname = new QualifiedName(n.getNamespaceURI(),
                        n.getLocalPart(), n.getPrefix());
                addTypename(qname);
            }
        }
    }

    public void addTypename(QualifiedName typename) {
		if (this.typenames == null) {
			this.typenames = new ArrayList<QualifiedName>();
		}
		this.typenames.add(typename);
	}

	/**
	 * @return the typenames
	 */
	public List<QualifiedName> getTypenames() {
		return typenames;
	}

	/**
	 * @param typenames
	 *            the typenames to set
	 */
	public void setTypenames(List<QualifiedName> typenames) {
		this.typenames = typenames;
	}

	/**
	 * @return the outputformat
	 */
	public String getOutputformat() {
		return outputformat;
	}

	/**
	 * @param outputformat
	 *            the outputformat to set
	 */
	public void setOutputformat(String outputformat) {
		this.outputformat = outputformat;
	}

}
