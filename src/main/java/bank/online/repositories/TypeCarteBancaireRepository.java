package bank.online.repositories;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import bank.online.entities.TypeCarteBancaire;

@Repository
public interface TypeCarteBancaireRepository extends JpaRepository<TypeCarteBancaire, Long> {

}
