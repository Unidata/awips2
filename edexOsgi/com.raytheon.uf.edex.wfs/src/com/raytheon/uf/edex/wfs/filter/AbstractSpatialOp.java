/*
 * The following software products were developed by Raytheon:
 *
 * ADE (AWIPS Development Environment) software
 * CAVE (Common AWIPS Visualization Environment) software
 * EDEX (Environmental Data Exchange) software
 * uFrame™ (Universal Framework) software
 *
 * Copyright (c) 2010 Raytheon Co.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/org/documents/epl-v10.php
 *
 *
 * Contractor Name: Raytheon Company
 * Contractor Address:
 * 6825 Pine Street, Suite 340
 * Mail Stop B8
 * Omaha, NE 68106
 * 402.291.0100
 *
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 21, 2011            bclement     Initial creation
 *
 */
package com.raytheon.uf.edex.wfs.filter;

import javax.xml.bind.JAXBElement;

import net.opengis.filter.v_1_1_0.BBOXType;
import net.opengis.filter.v_1_1_0.BinarySpatialOpType;
import net.opengis.filter.v_1_1_0.DistanceBufferType;
import net.opengis.filter.v_1_1_0.SpatialOpsType;

/**
 * 
 * @author bclement
 * @version 1.0
 */
public abstract class AbstractSpatialOp {

	public abstract Object visit(JAXBElement<? extends SpatialOpsType> op,
			OgcFilterVisitor visitor, Object obj) throws Exception;

	public static class SpatialEquals extends AbstractSpatialOp {
		@Override
		public Object visit(JAXBElement<? extends SpatialOpsType> op,
				OgcFilterVisitor visitor, Object obj) throws Exception {
			BinarySpatialOpType binary = (BinarySpatialOpType) op.getValue();
			return visitor.spatialEquals(binary, obj);
		}
	}

	public static class Disjoint extends AbstractSpatialOp {
		@Override
		public Object visit(JAXBElement<? extends SpatialOpsType> op,
				OgcFilterVisitor visitor, Object obj) throws Exception {
			BinarySpatialOpType binary = (BinarySpatialOpType) op.getValue();
			return visitor.disjoint(binary, obj);
		}
	}

	public static class Touches extends AbstractSpatialOp {
		@Override
		public Object visit(JAXBElement<? extends SpatialOpsType> op,
				OgcFilterVisitor visitor, Object obj) throws Exception {
			BinarySpatialOpType binary = (BinarySpatialOpType) op.getValue();
			return visitor.touches(binary, obj);
		}
	}

	public static class Within extends AbstractSpatialOp {
		@Override
		public Object visit(JAXBElement<? extends SpatialOpsType> op,
				OgcFilterVisitor visitor, Object obj) throws Exception {
			BinarySpatialOpType binary = (BinarySpatialOpType) op.getValue();
			return visitor.within(binary, obj);
		}
	}

	public static class Overlaps extends AbstractSpatialOp {
		@Override
		public Object visit(JAXBElement<? extends SpatialOpsType> op,
				OgcFilterVisitor visitor, Object obj) throws Exception {
			BinarySpatialOpType binary = (BinarySpatialOpType) op.getValue();
			return visitor.overlaps(binary, obj);
		}
	}

	public static class Crosses extends AbstractSpatialOp {
		@Override
		public Object visit(JAXBElement<? extends SpatialOpsType> op,
				OgcFilterVisitor visitor, Object obj) throws Exception {
			BinarySpatialOpType binary = (BinarySpatialOpType) op.getValue();
			return visitor.crosses(binary, obj);
		}
	}

	public static class Intersects extends AbstractSpatialOp {
		@Override
		public Object visit(JAXBElement<? extends SpatialOpsType> op,
				OgcFilterVisitor visitor, Object obj) throws Exception {
			BinarySpatialOpType binary = (BinarySpatialOpType) op.getValue();
			return visitor.intersects(binary, obj);
		}
	}

	public static class Contains extends AbstractSpatialOp {
		@Override
		public Object visit(JAXBElement<? extends SpatialOpsType> op,
				OgcFilterVisitor visitor, Object obj) throws Exception {
			BinarySpatialOpType binary = (BinarySpatialOpType) op.getValue();
			return visitor.contains(binary, obj);
		}
	}

	public static class DWithin extends AbstractSpatialOp {
		@Override
		public Object visit(JAXBElement<? extends SpatialOpsType> op,
				OgcFilterVisitor visitor, Object obj) throws Exception {
			DistanceBufferType dist = (DistanceBufferType) op.getValue();
			return visitor.dWithin(dist, obj);
		}
	}

	public static class Beyond extends AbstractSpatialOp {
		@Override
		public Object visit(JAXBElement<? extends SpatialOpsType> op,
				OgcFilterVisitor visitor, Object obj) throws Exception {
			DistanceBufferType dist = (DistanceBufferType) op.getValue();
			return visitor.beyond(dist, obj);
		}
	}

	public static class Bbox extends AbstractSpatialOp {
		@Override
		public Object visit(JAXBElement<? extends SpatialOpsType> op,
				OgcFilterVisitor visitor, Object obj) throws Exception {
			BBOXType bbox = (BBOXType) op.getValue();
			return visitor.bbox(bbox, obj);
		}
	}
}
