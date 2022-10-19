package bank.online.repositories;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import bank.online.entities.IntervalleAge;

@Repository
public interface IntervalleAgeRepository extends JpaRepository<IntervalleAge, Long>{

}
