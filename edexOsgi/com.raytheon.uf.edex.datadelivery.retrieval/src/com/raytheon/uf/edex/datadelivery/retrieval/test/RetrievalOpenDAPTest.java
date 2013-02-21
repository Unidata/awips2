package com.raytheon.uf.edex.datadelivery.retrieval.test;

import java.io.File;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

import org.opengis.coverage.Coverage;

import com.raytheon.uf.common.datadelivery.registry.Connection;
import com.raytheon.uf.common.datadelivery.registry.Time;
import com.raytheon.uf.common.datadelivery.retrieval.xml.Retrieval;
import com.raytheon.uf.common.datadelivery.retrieval.xml.RetrievalAttribute;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.gridcoverage.LambertConformalGridCoverage;
import com.raytheon.uf.common.gridcoverage.LatLonGridCoverage;
import com.vividsolutions.jts.geom.Coordinate;

public class RetrievalOpenDAPTest {

    private Retrieval prXML = null;

    public RetrievalOpenDAPTest(Retrieval prXML) {
        this.prXML = prXML;
    }

    public RetrievalOpenDAPTest() {
        String filePath = "/awipscm/dhladky/OpenDapNOMADS.xml";
        Retrieval prXML = null;

        try {
            @SuppressWarnings("rawtypes")
            Class[] clazzes = new Class[] { Retrieval.class,
                    RetrievalAttribute.class, Connection.class,
                    GridCoverage.class, LatLonGridCoverage.class,
                    LambertConformalGridCoverage.class, Coverage.class,
                    GridCoverage.class, Coordinate.class, Time.class };

            JAXBContext jax = JAXBContext.newInstance(clazzes);
            // Marshaller marsh = jax.createMarshaller();
            Unmarshaller umarsh = jax.createUnmarshaller();
            // marsh(ret, new File(filePath));
            prXML = (Retrieval) umarsh.unmarshal(new File(filePath));
        } catch (JAXBException e1) {
            e1.printStackTrace();
        }

        // try {

        // SerializationUtil.jaxbMarshalToXmlFile(ret, filePath);

        // prXML = (ProviderRetrievalXML) SerializationUtil
        // .jaxbUnmarshalFromXmlFile(filePath);
        // } catch (SerializationException e) {
        // e.printStackTrace();
        // }

        RetrievalOpenDAPTest prot = new RetrievalOpenDAPTest(prXML);
        prot.test();
    }

    private void test() {

        System.out
                .println("Changed to database!  Need to re-architect test driver");
        // handler.process(prXML);
    }

    /**
     * @param args
     *            data_store dir,
     */
    public static void main(String[] args) {

        String filePath = args[0];
        Retrieval prXML = null;

        try {
            @SuppressWarnings("rawtypes")
            Class[] clazzes = new Class[] { Retrieval.class,
                    RetrievalAttribute.class, Connection.class,
                    GridCoverage.class, LatLonGridCoverage.class,
                    LambertConformalGridCoverage.class, Coverage.class,
                    GridCoverage.class, Coordinate.class, Time.class };

            JAXBContext jax = JAXBContext.newInstance(clazzes);
            // Marshaller marsh = jax.createMarshaller();
            Unmarshaller umarsh = jax.createUnmarshaller();
            // marsh(ret, new File(filePath));
            prXML = (Retrieval) umarsh.unmarshal(new File(filePath));
        } catch (JAXBException e1) {
            e1.printStackTrace();
        }

        // try {

        // SerializationUtil.jaxbMarshalToXmlFile(ret, filePath);

        // prXML = (ProviderRetrievalXML) SerializationUtil
        // .jaxbUnmarshalFromXmlFile(filePath);
        // } catch (SerializationException e) {
        // e.printStackTrace();
        // }

        RetrievalOpenDAPTest prot = new RetrievalOpenDAPTest(prXML);
        prot.test();
    }
}
