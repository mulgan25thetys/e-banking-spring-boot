package bank.online.repositories;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import bank.online.entities.ProjetImmobilier;

@Repository
public interface ProjetImmobilierRepository extends JpaRepository<ProjetImmobilier, Long>{

}
