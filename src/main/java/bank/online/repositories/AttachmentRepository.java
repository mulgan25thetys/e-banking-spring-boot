package bank.online.repositories;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import bank.online.entities.Attachements;

@Repository
public interface AttachmentRepository extends JpaRepository<Attachements, Long>{

}
