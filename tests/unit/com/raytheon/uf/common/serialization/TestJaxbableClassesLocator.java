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
package com.raytheon.uf.common.serialization;

import java.util.Arrays;
import java.util.List;

import javax.xml.ws.wsaddressing.W3CEndpointReference;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ExtensibleObjectType;

import org.junit.Ignore;

import com.raytheon.uf.common.monitor.xml.FFMPTemplateXML;
import com.raytheon.uf.edex.registry.ebxml.services.query.adhoc.AdhocQueryExpression;

/**
 * Implementation of {@link IJaxbableClassesLocator} that returns a static list
 * of classes.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 06, 2013 1543       djohnson     Moved out of SerializationUtilTest.
 * Apr 17, 2013 1914       djohnson     Add W3CEndpointReference.
 * May 29, 2013 1965       bgonzale     Added archive classes.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
@Ignore
public class TestJaxbableClassesLocator implements IJaxbableClassesLocator {

    /**
     * An ever-growing list of JAXB-able classes. Basically this array should
     * duplicate the contents of the ISerializableContext files at some point
     * (as they are needed).
     */
    @SuppressWarnings("rawtypes")
    private static final List JAXB_CLASSES;
    static {
        Class<?>[] array = new Class<?>[] {
                com.raytheon.uf.common.archive.config.ArchiveConfig.class,
                com.raytheon.uf.common.archive.config.CategoryConfig.class,
                com.raytheon.edex.plugin.redbook.decoder.RedbookFcstMap.class,
                com.raytheon.uf.common.datadelivery.registry.AdhocSubscription.class,
                com.raytheon.uf.common.datadelivery.registry.Connection.class,
                com.raytheon.uf.common.datadelivery.registry.Coverage.class,
                com.raytheon.uf.common.datadelivery.registry.DataLevelType.class,
                com.raytheon.uf.common.datadelivery.registry.DataSet.class,
                com.raytheon.uf.common.datadelivery.registry.DataSetMetaData.class,
                com.raytheon.uf.common.datadelivery.registry.DataSetName.class,
                com.raytheon.uf.common.datadelivery.registry.Ensemble.class,
                com.raytheon.uf.common.datadelivery.registry.GriddedCoverage.class,
                com.raytheon.uf.common.datadelivery.registry.GriddedDataSet.class,
                com.raytheon.uf.common.datadelivery.registry.GriddedDataSetMetaData.class,
                com.raytheon.uf.common.datadelivery.registry.GriddedProjection.class,
                com.raytheon.uf.common.datadelivery.registry.GroupDefinition.class,
                com.raytheon.uf.common.datadelivery.registry.Levels.class,
                com.raytheon.uf.common.datadelivery.registry.OpenDapGriddedDataSet.class,
                com.raytheon.uf.common.datadelivery.registry.OpenDapGriddedDataSetMetaData.class,
                com.raytheon.uf.common.datadelivery.registry.Parameter.class,
                com.raytheon.uf.common.datadelivery.registry.ParameterLevel.class,
                com.raytheon.uf.common.datadelivery.registry.PendingSharedSubscription.class,
                com.raytheon.uf.common.datadelivery.registry.PendingSiteSubscription.class,
                com.raytheon.uf.common.datadelivery.registry.Projection.class,
                com.raytheon.uf.common.datadelivery.registry.Provider.class,
                com.raytheon.uf.common.datadelivery.registry.SharedSubscription.class,
                com.raytheon.uf.common.datadelivery.registry.SiteSubscription.class,
                com.raytheon.uf.common.datadelivery.registry.Time.class,
                com.raytheon.uf.common.datadelivery.retrieval.xml.ParameterLookup.class,
                com.raytheon.uf.common.datadelivery.retrieval.xml.ParameterConfig.class,
                com.raytheon.uf.edex.datadelivery.bandwidth.dao.BandwidthDataSetUpdate.class,
                com.raytheon.uf.edex.datadelivery.bandwidth.dao.BandwidthSubscription.class,
                com.raytheon.uf.edex.datadelivery.bandwidth.dao.BandwidthAllocation.class,
                com.raytheon.uf.common.datadelivery.bandwidth.data.BandwidthMap.class,
                com.raytheon.uf.common.datadelivery.retrieval.xml.ServiceConfig.class,
                com.raytheon.uf.common.datadelivery.retrieval.xml.UnitLookup.class,
                com.raytheon.uf.common.datadelivery.retrieval.xml.LevelLookup.class,
                com.raytheon.uf.common.monitor.xml.FFMPSourceConfigXML.class,
                com.raytheon.uf.common.monitor.xml.FFMPRunConfigXML.class,
                FFMPTemplateXML.class, W3CEndpointReference.class,
                AdhocQueryExpression.class, ExtensibleObjectType.class };

        JAXB_CLASSES = Arrays.asList(array);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    @SuppressWarnings("unchecked")
    public List<Class<ISerializableObject>> getJaxbables() {
        return JAXB_CLASSES;
    }
}
