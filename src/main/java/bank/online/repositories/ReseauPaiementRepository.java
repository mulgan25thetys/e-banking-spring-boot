package bank.online.repositories;

import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import bank.online.entities.ReseauPaiement;

@Repository
public interface ReseauPaiementRepository extends JpaRepository<ReseauPaiement, Long>{

	Boolean existsByNom(String nom);

	Optional<ReseauPaiement> findByNom(String nom);
}
