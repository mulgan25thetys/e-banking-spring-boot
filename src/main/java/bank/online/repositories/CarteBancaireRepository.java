package bank.online.repositories;

import java.util.List;
import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import bank.online.entities.CarteBancaire;

@Repository
public interface CarteBancaireRepository extends JpaRepository<CarteBancaire, Long> {

	@Query(value = "SELECT * FROM carte_bancaire cb INNER JOIN user_carte_bancaires ucb ON ucb.carte_bancaires_id_carteb = cb.id_carteb "
			+ "INNER JOIN user u ON u.id = ucb.user_id WHERE u.id =:idUser",nativeQuery = true)
	List<CarteBancaire> getCardsByUser(@Param("idUser") Long idUser);
	
	@Query(value = "SELECT * FROM carte_bancaire cb "
			+ "INNER JOIN type_carte_bancaire tcb ON cb.id_carteb = tcb.id_type_carte "
			+ "WHERE tcb.nom =:type",nativeQuery = true)
	List<CarteBancaire> getCardsByType(@Param("type") String type);
	
	@Query(value = "SELECT * FROM carte_bancaire cb INNER JOIN  user_carte_bancaires ucb "
			+ "ON cb.id_carteb = ucb.carte_bancaires_id_carteb WHERE cb.id_carteb = :idCard",nativeQuery = true)
	Optional<CarteBancaire> getIfSubscribedCard(@Param("idCard") Long idCard);
	
	@Query(value = "SELECT * FROM carte_bancaire cb INNER JOIN user_carte_bancaires ucb ON ucb.carte_bancaires_id_carteb = cb.id_carteb INNER JOIN user u ON u.id = ucb.user_id WHERE u.id =:idUser AND cb.numero =:number",nativeQuery = true)
	Optional<CarteBancaire> getCardByNumber(@Param("number") String number,@Param("idUser") Long idUser);
	
	@Query(value = "SELECT * FROM carte_bancaire WHERE est_soucrite =0 OR est_soucrite is null",nativeQuery = true)
	List<CarteBancaire> getNonSubscribed();
	
	@Query(value = "SELECT CASE WHEN SUM(tacb.provision) IS null THEN 0 ELSE SUM(tacb.provision) END FROM user_carte_bancaires ucb INNER JOIN "
			+ "carte_bancaire cb ON ucb.carte_bancaires_id_carteb = cb.id_carteb INNER JOIN "
			+ "type_carte_bancaire tcb ON cb.type_carte_id_type_carte = tcb.id_type_carte "
			+ "INNER JOIN tarif_carte_bancaire tacb ON tcb.tarif_id_tarif = tacb.id_tarif "
			+ "WHERE ucb.user_id = :idUser",nativeQuery = true)
	float getTotalCapital(@Param("idUser") Long idUser);
	
	@Query(value = "SELECT cb.numero FROM carte_bancaire cb INNER JOIN "
			+ "user_carte_bancaires ucb ON cb.id_carteb = ucb.carte_bancaires_id_carteb "
			+ "INNER JOIN user usr ON usr.id = ucb.user_id WHERE usr.id=:idUser",nativeQuery =true)
	List<String> getLowCardNumber(@Param("idUser") Long idUser);
}
