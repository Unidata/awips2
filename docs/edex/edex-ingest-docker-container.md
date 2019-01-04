Project home: [https://github.com/Unidata/edex-docker](https://github.com/Unidata/edex-docker)

---

EDEX can be run inside a docker container, which allows you to process data into an AWIPS system without requiring a full CentOS installation and configuration.

The [EDEX Docker Image](https://github.com/Unidata/edex-docker) is built on CentOS 7 and contains the latest Unidata AWIPS release.  

This container is an *ingest-only* install, meaning there is *no database or request server*.  This example requires a Database/Request server be configured for you to access remotely.  See the [Distributed EDEX](https://unidata.github.io/awips2/edex/distributed-computing/) document for more. 

---

## Download and Install Docker

Download and install Docker and Docker Compose:

* [Docker for CentOS 7 Linux](https://docs.docker.com/install/linux/docker-ce/centos/)
* [Docker for Mac](https://docs.docker.com/docker-for-mac/)
* [Docker for Windows](https://docs.docker.com/docker-for-windows/install/)
* [docker-compose](https://docs.docker.com/compose/) (it should be bundled with Docker by default on Mac and Windows)

## Run the EDEX Ingest Container

Clone this repository

    git clone https://github.com/Unidata/edex-docker.git
    cd edex-docker

Run the container with docker-compose

    docker-compose up -d edex-ingest

Confirm the container is running

    docker ps -a 

Enter the container

    docker exec -it edex-ingest bash    

Stop the container

    docker-compose stop

Delete the container (keep the image)

    docker-compose rm -f
    
Run commands inside the container, such as

    docker exec edex-ingest edex

which should return something like

    [edex status]
     qpid        :: running :: pid 22474
     EDEXingest  :: running :: pid 21860 31513
     EDEXgrib    :: not running
     ldmadmin    :: running :: pid 22483

     edex (status|start|stop|setup|log|purge|qpid|users)

To update to the latest version and restart:

```bash
docker pull unidata/edex-ingest:latest
docker-compose stop
docker-compose up -d edex-ingest
```

## Configuration and Customization

The file `docker-compose.yml` defines files to mount to the container and which ports to open:

    edex-ingest:
      image: unidata/edex-ingest:latest
      container_name: edex-ingest
      volumes:
        - ./etc/ldmd.conf:/awips2/ldm/etc/ldmd.conf
        - ./etc/pqact.conf:/awips2/ldm/etc/pqact.conf
        - ./bin/setup.env:/awips2/edex/bin/setup.env
        - ./bin/runedex.sh:/awips2/edex/bin/runedex.sh
      ports:
        - "388:388"
      ulimits:
        nofile:
          soft: 1024
          hard: 1024

## Mounted Files

- `etc/ldmd.conf`

    Defines which data feeds to receive. By default there is only one active request line (`REQUEST IDS|DDPLUS ".*" idd.unidata.ucar.edu`) to not overwhelm small EDEX containers ingesting large volumes of radar and gridded data files.  Any updates to the file `etc/ldmd.conf` will be read the next time you restart the container.
 
- `etc/pqact.conf`

    Defines how products are processed and where they are written to on the filesystem. This is the full set of pattern actions used in Unidata AWIPS, and generally you do not need to edit this file. Instead control which data feeds are requested in `ldmd.conf` (above).

- `bin/setup.env`

    Defines the remote EDEX Database/Request server:
    
        ### EDEX localization related variables ###
        export AW_SITE_IDENTIFIER=OAX
        export EXT_ADDR=js-157-198.jetstream-cloud.org

    **EXT_ADDR** must be set to an allowed EDEX Database/Request Server. In this example we are using a JetStream Cloud instance, which controls our *edex-ingest* access with IPtables, SSL certificates, and PostgreSQL **pg_hba.conf** rules (this server is used in software training workshop environments and will not allow outside connections). 

- `bin/runedex.sh`

    The default script run when the container is started, acts as a sort-of service manager for EDEX and the LDM (see `ENTRYPOINT ["/awips2/edex/bin/runedex.sh"]` in *Dockerfile.edex*), essentially:

        /awips2/qpid/bin/qpid-wrapper &
        /awips2/edex/bin/start.sh -noConsole ingest &
        ldmadmin mkqueue
        ldmadmin start
        
