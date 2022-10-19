package bank.online.repositories;

import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import bank.online.entities.Devises;

@Repository
public interface DevisesRepository extends JpaRepository<Devises, Long> {

	Boolean existsByNom(String nom);
	
	Boolean existsByIndice(String indice);
	
	Optional<Devises> findByNom(String nom);
	
	Optional<Devises> findByIndice(String indice);
}
