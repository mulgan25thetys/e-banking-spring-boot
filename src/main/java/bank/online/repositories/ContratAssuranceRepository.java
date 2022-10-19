package bank.online.repositories;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import bank.online.entities.ContratAssurance;

@Repository
public interface ContratAssuranceRepository extends JpaRepository<ContratAssurance, Long> {

}
