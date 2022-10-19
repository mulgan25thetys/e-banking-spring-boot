package bank.online.repositories;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import bank.online.entities.InformationComplementaire;

@Repository
public interface InformationComplementaireRepository  extends JpaRepository<InformationComplementaire, Long>{

}
