package bank.online.repositories;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import bank.online.entities.ProduitImmobilier;

@Repository
public interface ProduitImmobilierRepository extends JpaRepository<ProduitImmobilier, Long>{

}
