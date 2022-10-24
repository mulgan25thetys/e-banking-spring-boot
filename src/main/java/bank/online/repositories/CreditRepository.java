package bank.online.repositories;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import bank.online.entities.Credit;

@Repository
public interface CreditRepository extends JpaRepository<Credit, Long>{

}
