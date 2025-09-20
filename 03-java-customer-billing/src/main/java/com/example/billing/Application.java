package com.example.billing;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;


/**
 * Main entry point for the Java Customer Billing Spring Boot application.
 * <p>
 * This class bootstraps the Spring application context.
 */
@SpringBootApplication
public class Application {

    /**
     * Starts the Spring Boot application.
     *
     * @param args command-line arguments passed to the application
     */
    public static void main(String[] args) {
        SpringApplication.run(Application.class, args);
    }
}
