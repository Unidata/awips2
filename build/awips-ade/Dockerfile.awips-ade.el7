FROM unidata/awips-devel:el7
ENV VERSION 18.1.1
ENV RELEASE 6
MAINTAINER Michael James <mjames@ucar.edu>

USER root

COPY el7-dev.repo /etc/yum.repos.d/awips2.repo

RUN groupadd fxalpha && useradd -G fxalpha awips

RUN yum -y clean all
RUN yum groupinstall awips2-ade -y

RUN mkdir -p /awips2/jenkins/buildspace/workspace/AWIPS2-UPC_build/baseline && mkdir -p /awips2/jenkins/buildspace/workspace/tmp
RUN mkdir -p /awips2/jenkins/build/rpms/awips2_latest/{x86_64,noarch}/
RUN chown -R awips:fxalpha /awips2/jenkins/

ENTRYPOINT ["/bin/bash"]
