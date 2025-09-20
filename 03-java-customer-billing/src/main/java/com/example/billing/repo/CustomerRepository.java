package com.example.billing.repo;

import com.example.billing.domain.Customer;
import org.springframework.data.mongodb.repository.MongoRepository;


/**
 * Repository interface for accessing and managing {@link Customer} entities in MongoDB.
 * <p>
 * Extends {@link MongoRepository} to provide CRUD operations and query methods.
 */
public interface CustomerRepository extends MongoRepository<Customer, String> {
}
