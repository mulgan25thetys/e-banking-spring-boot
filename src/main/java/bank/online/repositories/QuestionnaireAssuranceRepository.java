package bank.online.repositories;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import bank.online.entities.QuestionnaireAssurance;

@Repository
public interface QuestionnaireAssuranceRepository extends JpaRepository<QuestionnaireAssurance, Long> {

}
