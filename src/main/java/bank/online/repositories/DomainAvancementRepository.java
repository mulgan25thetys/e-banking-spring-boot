package bank.online.repositories;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import bank.online.entities.DomaineAvancement;

@Repository
public interface DomainAvancementRepository extends JpaRepository<DomaineAvancement, Long> {

}
