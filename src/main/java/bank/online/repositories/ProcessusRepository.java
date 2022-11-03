package bank.online.repositories;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import bank.online.entities.Processus;

@Repository
public interface ProcessusRepository extends JpaRepository<Processus, Long>{

}
