package bank.online.repositories;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import bank.online.entities.Chat;

@Repository
public interface ChatRepository extends JpaRepository<Chat, Long>{

	Boolean existsByIncomeId(Long incomeId);
	
	Boolean existsByOutgoingId(Long outgoingId);
	
	@Query(value = "SELECT *FROM Chat WHERE income_id=:uniqueId OR outgoing_id=:uniqueId ORDER BY id ASC",nativeQuery = true)
	List<Chat> getUserMessages(@Param("uniqueId") Long id);
}
