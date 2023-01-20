FROM openjdk:8
VOLUME /tmp
EXPOSE 8085
ADD ./target/e-banking-spring-boot-0.0.1-SNAPSHOT.jar e-banking-spring-boot.jar
ENTRYPOINT ["java","-jar","e-banking-spring-boot.jar"]