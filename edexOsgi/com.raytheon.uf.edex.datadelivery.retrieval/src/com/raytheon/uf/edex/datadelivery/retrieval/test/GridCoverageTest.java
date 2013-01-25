package com.raytheon.uf.edex.datadelivery.retrieval.test;

import java.io.File;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

import com.raytheon.uf.common.datadelivery.registry.Coverage;
import com.raytheon.uf.common.datadelivery.registry.GriddedCoverage;
import com.raytheon.uf.common.datadelivery.registry.Levels;
import com.raytheon.uf.common.datadelivery.registry.Time;
import com.raytheon.uf.common.datadelivery.retrieval.xml.Retrieval;
import com.raytheon.uf.common.datadelivery.retrieval.xml.RetrievalAttribute;
import com.raytheon.uf.common.gridcoverage.Corner;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.gridcoverage.LambertConformalGridCoverage;
import com.raytheon.uf.common.gridcoverage.LatLonGridCoverage;
import com.raytheon.uf.common.gridcoverage.exception.GridCoverageException;

public class GridCoverageTest {

    public GridCoverageTest() {

    }

    private GriddedCoverage test() {

        GriddedCoverage rgc = new GriddedCoverage();
        Levels levs = new Levels();
        levs.setDz(-25.0);
        // levs.setRequestLevelEnd(500.0);
        // levs.setRequestLevelStart(1000.0);
        levs.setLevelType(100);
        levs.addLevel(1000.0);
        levs.addLevel(900.0);
        levs.addLevel(800.0);
        levs.addLevel(700.0);
        // rgc.setLevels(levs);

        GridCoverage coverage = new LatLonGridCoverage();
        coverage.setDx(0.113);
        coverage.setDy(0.111);
        coverage.setNx(912);
        coverage.setNy(443);
        coverage.setLa1(61.17565454545);
        coverage.setLo1(-152.87862300000);
        coverage.setSpacingUnit("degree");
        coverage.setFirstGridPointCorner(Corner.UpperLeft);
        String nameAndDescription = "Retrieval Test Coverage "
                + coverage.getNx() + " X " + coverage.getNy() + " "
                + coverage.getProjectionType() + " grid";
        coverage.setName(nameAndDescription);

        try {
            coverage.initialize();
        } catch (GridCoverageException e) {
            e.printStackTrace();
        }

        rgc.setGridCoverage(coverage);

        return rgc;

    }

    /**
     * @param args
     *            data_store dir,
     */
    public static void main(String[] args) {

        String filePath = args[0];

        try {
            @SuppressWarnings("rawtypes")
            Class[] clazzes = new Class[] { Retrieval.class,
                    GridCoverage.class, LatLonGridCoverage.class,
                    LambertConformalGridCoverage.class,
                    RetrievalAttribute.class, Coverage.class,
                    GriddedCoverage.class, Time.class };

            GridCoverageTest test = new GridCoverageTest();
            GriddedCoverage coverage = test.test();

            JAXBContext jax = JAXBContext.newInstance(clazzes);
            Marshaller marsh = jax.createMarshaller();

            marsh.marshal(coverage, new File(filePath));
            Unmarshaller umarsh = jax.createUnmarshaller();
            GriddedCoverage coverage2 = (GriddedCoverage) umarsh
                    .unmarshal(new File(filePath));
            GriddedCoverage rgc = (GriddedCoverage) umarsh.unmarshal(new File(
                    filePath));
            System.out.println("DONE!");
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

    }

}
